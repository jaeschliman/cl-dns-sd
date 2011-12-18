#| This file is part of DNS-SD, a Common Lisp library for
   Zeroconf service discovery. |#

(in-package #:dns-sd)

;; --- Locks ---

;; Each Lisp implementation needs to provide a MAKE-LOCK function to
;; create a mutex, and a WITH-LOCK macro to use the mutex.  The mutex
;; must be recursive.

#+allegro
(progn
  (defun make-lock (name)
    (mp:make-process-lock :name name))

  (defmacro with-lock ((lock) &body body)
    `(mp:with-process-lock (,lock)
       ,@body)))

#+(or lispworks sbcl openmcl)
(progn
  (defun make-lock (name)
    (s-sysdeps:make-process-lock name))

  (defmacro with-lock ((lock) &body body)
    `(s-sysdeps:with-process-lock (,lock)
       ,@body)))

;; --- I/O ---

;; Each implementation needs to define a function
;; FDS-INPUT-AVAILABLE-P which acts a little like the UNIX select(2)
;; system call.  It must take a list of file descriptors and an
;; optional timeout, and return the subset of the descriptors for
;; which input is available (or the empty list if the timeout expires
;; without any descriptor being ready for input).
;;
;; Most implementations of this function for different Lisps fall into
;; two categories: they either use the UNIX select(2) system call or
;; they poll all descriptors, sleep for a short time, then loop.  The
;; select(2) method should be more efficient, but is less portable (in
;; particular for Windows).

;; The SBCL version uses select(2)
#+sbcl
(defun fds-input-available-p (fd-list &optional timeout)
  (if (null fd-list)
      '()
      (multiple-value-bind (secs usecs)
          (sb-impl::decode-timeout (and timeout (/ timeout 1000.0)))
        (sb-alien:with-alien ((read-fds (sb-alien:struct sb-unix:fd-set))
                              (write-fds (sb-alien:struct sb-unix:fd-set))
                              (error-fds (sb-alien:struct sb-unix:fd-set)))
          (sb-unix:fd-zero read-fds)
          (sb-unix:fd-zero write-fds)
          (sb-unix:fd-zero error-fds)
          (dolist (fd fd-list)
            (sb-unix:fd-set fd read-fds)
            (sb-unix:fd-set fd error-fds))
          (multiple-value-bind (value error)
              (sb-unix:unix-fast-select (1+ (reduce #'max fd-list))
                                        (sb-alien:addr read-fds)
                                        (sb-alien:addr write-fds)
                                        (sb-alien:addr error-fds)
                                        secs usecs)
            (cond ((eql value 0)
                   ;; The select timed out.
                   '())
                  ((and value (> value 0))
                   ;; There's activity on at least one fd.
                   (remove-if-not #'(lambda (fd)
                                      (or (sb-unix:fd-isset fd read-fds)
                                          (sb-unix:fd-isset fd error-fds)))
                                  fd-list))
                  ((eql error sb-posix:eintr)
                   ;; Got an interrupt, try again.  We do need to
                   ;; check for this, right?
                   (fds-input-available-p fd-list timeout))
                  (T
                   (error "unix-fast-select returned the error code ~S." error))))))))

;; We need this in order to do the (ccl::syscall os::select ...) stuff.
#+openmcl
(progn
  (eval-when (:load-toplevel :compile-toplevel)
    (require #+linuxppc-target 'linux-syscalls
	     #+darwinppc-target 'darwinppc-syscalls
             #+darwinx8664-target 'darwinx8664-syscalls
             #+darwinx8632-target 'darwinx8632-syscalls
	     ))

  ;; The OpenMCL implementation uses select(2).  This code is based on
  ;; the CCL::FD-INPUT-AVAILABLE-P function that's internal to OpenMCL
  ;; and handles single file descriptors.

  (defun fds-input-available-p (fd-list &optional timeout)
    (if (null fd-list)
	'()
	(ccl:rletZ ((tv :timeval))
	  (let ((ticks (if timeout
			   (ceiling (* timeout ccl::*ticks-per-second*))
			   nil)))
	    (ccl::ticks-to-timeval ticks tv))
	  (ccl:%stack-block ((infds ccl::*fd-set-size*)
			     (errfds ccl::*fd-set-size*))
	    (ccl::fd-zero infds)
	    (ccl::fd-zero errfds)
	    (dolist (fd fd-list)
	      (ccl::fd-set fd infds)
	      (ccl::fd-set fd errfds))
	    (let* ((result (ccl::syscall syscalls::select
					 (1+ (reduce #'max fd-list)) infds
					 (ccl:%null-ptr) errfds
					 (if timeout tv (ccl:%null-ptr)))))
	      (cond ((eql result 0)
		     ;; The select timed out.
		     '())
		    ((and result (> result 0))
		     ;; There's activity on at least one fd.
		     (remove-if-not #'(lambda (fd)
					(or (ccl::fd-is-set fd infds)
					    (ccl::fd-is-set fd errfds)))
				    fd-list))
		    ((eql (abs result) #.(read-from-string "#$EINTR"))
		     (format *trace-output* "Got EINTR in select syscall.~%")
		     ;; Got an interrupt, try again.  I'm no UNIX
		     ;; expert, is this check really required?
		     (fds-input-available-p fd-list timeout))
		    (t
		     (error "Select returned the error code ~s." result)))))))))

;; The LispWorks implementation uses the polling approach, using the
;; SOCKET-LISTEN function internal to the COMM package.
#+lispworks
(progn
  (require "comm")

  (defun fds-input-available-p (fd-list &optional timeout)
    (if (and timeout (<= timeout 0))
	'()
	(if (null fd-list)
	    '()
	    (let ((ready-fds (remove-if-not #'comm::socket-listen fd-list)))
	      (if ready-fds
		  ready-fds
		  (progn
		    (sleep 0.1)
		    (fds-input-available-p
		     fd-list
		     (if timeout (- timeout 0.1) nil)))))))))

;; The ACL implementation uses a polling approach.
#+allegro
(defun fds-input-available-p (fd-list &optional timeout)
  (if (or (and timeout (<= timeout 0)) (null fd-list))
      '()
      (let ((ready-fds (remove-if-not #'excl:stream-listen fd-list)))
	(if ready-fds
	    ready-fds
	    (progn
	      (sleep 0.1)
	      (fds-input-available-p
	       fd-list (if timeout (- timeout 0.1) nil)))))))
