#| DNS-SD, a Common Lisp library for Zeroconf service discovery.
   ------
   This file shows a basic usage example.

   You can also watch the offer and withdrawal of services by executing:

   dns-sd _http._tcp (Mac OS X)
   avahi-browse _http._tcp (platforms supported by Avahi) |#

(in-package #:cl-user)

(defclass basic-observer ()
  ())

(defmethod dns-sd:browse-add-service ((self basic-observer) service interface-index more-coming-p)
  (declare (ignore more-coming-p))
  (format t "~&Found service ~s.~%~%" service)
  (dns-sd:resolve service self))

(defmethod dns-sd:browse-remove-service ((self basic-observer) service interface-index more-coming-p)
  (declare (ignore more-coming-p))
  (format t "~&Service ~s went away.~%~%" service))

(defmethod dns-sd:browse-error ((self basic-observer) error)
  (format t "~&The following error occurred while browsing: ~s~%" error))

(defmethod dns-sd:browse-resolved-service ((self basic-observer) service)
  (format t "~&Resolved service ~s~%~%" service))

;;; Evaluate the following expressions at will, so that you test
;;; publishing, browsing, resolving and so on as you like

(defparameter *http-service*
  (make-instance 'dns-sd:service
		 :name "Test"
		 :type "_http._tcp"
		 :port 8080
		 :txt-record (dns-sd:build-txt-record '(("path" . "/")))))

(defparameter *http-browser* (dns-sd:browse "_http._tcp" "" (make-instance 'basic-observer)))
(dns-sd:process-dns-sd-events 1.0)

;; Register service
(dns-sd:publish *http-service* nil)
(dns-sd:process-dns-sd-events 1.0)

;; Unregister service
(dns-sd:cancel *http-service*)
(dns-sd:process-dns-sd-events 1.0)

;; Cancel browsing
(dns-sd:cancel *http-browser*)
(dns-sd:process-dns-sd-events 1.0)
