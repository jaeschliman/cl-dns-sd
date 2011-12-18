#| This file is part of DNS-SD, a Common Lisp library for
   Zeroconf service discovery.

   This file contains the necessary support for callbacks from foreign
   code.  Thanks to CFFI this is implementation independent. |#

(in-package #:dns-sd)

(cffi:defcallback %publish-callback-trampoline :void
    ((oid dns-service-ref)
     (flags dns-service-flags)
     (error-code dns-service-error-type)
     (name :pointer)
     (type :pointer)
     (domain :pointer)
     (context :pointer))
  (handler-case
      (publish-callback-trampoline oid flags error-code
                                   (cffi:foreign-string-to-lisp name)
                                   (cffi:foreign-string-to-lisp type)
                                   (cffi:foreign-string-to-lisp domain)
                                   context)
    (error (c) ; prevent non-local exits from unwinding the C stack
      (format *error-output*
              "Unexpected error while invoking publish-callback-trampoline: ~a" c))))

(cffi:defcallback %browse-callback-trampoline :void
    ((oid dns-service-ref)
     (flags dns-service-flags)
     (interface-index :unsigned-long)
     (error-code dns-service-error-type)
     (name :pointer)
     (type :pointer)
     (domain :pointer)
     (context :pointer))
  (handler-case
      (browse-callback-trampoline oid flags interface-index error-code
                                  (cffi:foreign-string-to-lisp name)
                                  (cffi:foreign-string-to-lisp type)
                                  (cffi:foreign-string-to-lisp domain)
                                  context)
    (error (c) ; prevent non-local exits from unwinding the C stack
      (format *error-output*
              "Unexpected error while invoking publish-callback-trampoline: ~a" c))))

(cffi:defcallback %resolve-callback-trampoline :void
    ((oid dns-service-ref)
     (flags dns-service-flags)
     (interface-index :uint32)
     (error-code dns-service-error-type)
     (full-name :pointer)
     (host-target :pointer)
     (port :unsigned-short)
     (txt-len :unsigned-short)
     (txt-record :pointer)
     (context :pointer))
  (handler-case
      (resolve-callback-trampoline oid flags interface-index error-code
                                  (cffi:foreign-string-to-lisp full-name)
                                  (cffi:foreign-string-to-lisp host-target)
                                  port txt-len txt-record context)
    (error (c) ; prevent non-local exits from unwinding the C stack
      (format *error-output*
              "Unexpected error while invoking publish-callback-trampoline: ~a" c))))

(cffi:defcallback %query-callback-trampoline :void
    ((oid dns-service-ref)
     (flags dns-service-flags)
     (interface-index :unsigned-long)
     (error-code dns-service-error-type)
     (full-name :pointer)
     (rrtype :unsigned-short)
     (rrclass :unsigned-short)
     (rdlen :unsigned-short)
     (rdata :pointer)
     (ttl :unsigned-short)
     (context :pointer))
  (handler-case
      (query-callback-trampoline oid flags interface-index error-code
                                  (cffi:foreign-string-to-lisp full-name)
                                  rrtype rrclass rdlen rdata ttl context)
    (error (c) ; prevent non-local exits from unwinding the C stack
      (format *error-output*
              "Unexpected error while invoking publish-callback-trampoline: ~a" c))))
