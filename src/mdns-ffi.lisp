#| This file is part of DNS-SD, a Common Lisp library for
   Zeroconf service discovery.

   This file contains the CFFI definitions for Apple's
   DNSServiceDiscovery API |#

(in-package #:dns-sd)

;; Load the host system's DNS-SD library

#-(or darwin-target darwin)
(handler-case
    (progn
      (cffi:define-foreign-library dns-sd
        (:unix (:default "libdns_sd")))
      (cffi:use-foreign-library dns-sd))
  (error (c)
    (warn "Couldn't load mDNSResponder client library; ~@
           DNS-SD functionality won't be available"))
  (:no-error (result)
    (when result
      (pushnew :dns-sd *features*))))

#+(or darwin-target darwin)
(pushnew :dns-sd *features*) ; mDNSResponder is included in Mac OS X

;; ----------
;; CFFI definitions and utilities
;; ----------

;; Some convenience macros.

(defmacro def-dnssd-type (type foreign-type)
  "Defines a foreign (CFFI) type TYPE and defines a Lisp type
   corresponding to the foreign type."
  `(cffi:defctype ,type ,foreign-type))

(defmacro def-dnssd-function (name args external-name)
  "Declares a foreign (CFFI) function that returns a value of
  type DNSServiceErrorType and defines a wrapper around the
  foreign function that raises an error of type
  DNS-SD-RESULT-ERROR if the foreign function returns a value
  indicating that an error occurred."
  (let ((arg-names (mapcar #'car args))
        (result-var (gensym "RESULT")))
    `(let ((cfun (symbol-function (cffi:defcfun ,external-name dns-service-error-type ,@args))))
       (defun ,name ,arg-names
         (let ((,result-var (funcall cfun ,@arg-names)))
           (if (= ,result-var 0)
               ,result-var
               (handle-dns-sd-error ,result-var)))))))

(defun handle-dns-sd-error (code)
  (dns-sd-error code))

;; Types (see dns_sd.h)

(def-dnssd-type dns-service-ref :pointer)
(def-dnssd-type dns-record-ref :pointer)

(def-dnssd-type dns-service-flags :uint32)
(def-dnssd-type dns-service-protocol :uint32)
(def-dnssd-type dns-service-error-type :int32)

(def-dnssd-type dns-service-register-reply :pointer)
(def-dnssd-type dns-service-browse-reply :pointer)
(def-dnssd-type dns-service-resolve-reply :pointer)
(def-dnssd-type dns-service-query-record-reply :pointer)

;; Functions

(cffi:defcfun ("DNSServiceRefSockFD"
               %dns-service-ref-sock-fd) :int
  (sd-ref dns-service-ref))

;; A wrapper around DNSServiceRefSockFD that raises a SOCKET-FD-ERROR
;; if the returned value is -1.  It seems that if the mDNS daemon
;; isn't running, the only indication we get is a -1 when we call
;; DNSServiceRefSockFD.

(defun dns-service-ref-sock-fd (ref)
  (let ((sock (%dns-service-ref-sock-fd ref)))
    (if (eql sock -1)
        (error 'socket-fd-error :oid ref)
        sock)))

(cffi:defcfun ("DNSServiceRefDeallocate"
               dns-service-ref-deallocate) :void
  (sd-ref dns-service-ref))

(def-dnssd-function dns-service-process-result ((sd-ref dns-service-ref))
  "DNSServiceProcessResult")

(def-dnssd-function dns-service-browse
    ((sd-ref-ptr :pointer)
     (flags dns-service-flags)
     (interface-index :uint32)
     (reg-type :string)
     (domain :string)
     (callback dns-service-browse-reply)
     (context :pointer))
  "DNSServiceBrowse")

(def-dnssd-function dns-service-resolve
    ((sd-ref-ptr :pointer)
     (flags dns-service-flags)
     (interface-index :uint32)
     (name :string)
     (reg-type :string)
     (domain :string)
     (callback dns-service-resolve-reply)
     (context :pointer))
  "DNSServiceResolve")

(def-dnssd-function dns-service-query-record
    ((sd-ref-ptr :pointer)
     (flags dns-service-flags)
     (interface-index :uint32)
     (full-name :string)
     (rrtype :uint16)
     (rrclass :uint16)
     (callback dns-service-query-record-reply)
     (context :pointer))
  "DNSServiceQueryRecord")

(def-dnssd-function %dns-service-register
    ((sd-ref-ptr :pointer)
     (flags dns-service-flags)
     (interface-index :uint32)
     (name :string)
     (reg-type :string)
     (domain :string)
     (host :string)
     (port :uint16)
     (txt-len :uint16)
     (txt-record :pointer)
     (callback dns-service-register-reply)
     (context :pointer))
  "DNSServiceRegister")

#+(or little-endian x86)
(defun uint16/network-byte-order (value)
    "big-endian-uint16"
  (let ((result 0))
    (declare (type (unsigned-byte 16) value result))
    (setf (ldb (byte 8 8) result) (ldb (byte 8 0) value)
          (ldb (byte 8 0) result) (ldb (byte 8 8) value))
    result))

#-(or little-endian x86)
(defmacro uint16/network-byte-order (value)
  value)

(defun dns-service-register
    (sd-ref-ptr flags interface-index name
     reg-type domain host port txt-len
     txt-record dns-service-register-reply
     context)
  (%dns-service-register
   sd-ref-ptr flags interface-index name reg-type domain host
   (uint16/network-byte-order port)
   txt-len txt-record dns-service-register-reply context))

;; Constants

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +dns-service-err-no-err+ 0)
  (defconstant +dns-service-err-unknown+ -65537)
  (defconstant +dns-service-err-no-such-name+ -65538)
  (defconstant +dns-service-err-no-memory+ -65539)
  (defconstant +dns-service-err-bad-param+ -65540)
  (defconstant +dns-service-err-bad-reference+ -65541)
  (defconstant +dns-service-err-bad-state+ -65542)
  (defconstant +dns-service-err-bad-flags+ -65543)
  (defconstant +dns-service-err-unsupported+ -65544)
  (defconstant +dns-service-err-not-initialized+ -65545)
  (defconstant +dns-service-err-already-registered+ -65547)
  (defconstant +dns-service-err-name-conflict+ -65548)
  (defconstant +dns-service-err-invalid+ -65549)
  (defconstant +dns-service-err-incompatible+ -65551)
  (defconstant +dns-service-err-bad-interface-index+ -65552)

  (defconstant +dns-service-flags-more-coming+ 1)
  (defconstant +dns-service-flags-finished+ 0)
  (defconstant +dns-service-flags-add+ 2)
  (defconstant +dns-service-flags-default+ 4)
  (defconstant +dns-service-flags-remove+ 0)
  (defconstant +dns-service-flags-no-auto-rename+ 8)
  (defconstant +dns-service-flags-auto-rename+ 0)
  (defconstant +dns-service-flags-shared+ 16)
  (defconstant +dns-service-flags-unique+ 32)
  (defconstant +dns-service-flags-browse-domains+ 64)
  (defconstant +dns-service-flags-registration-domains+ 128))
