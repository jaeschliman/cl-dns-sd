#| CL-ZEROCONF, a Lisp library for Zeroconf service discovery.

   Tests package. |#

(defpackage #:dns-sd-tests
  (:use #:common-lisp #:5am #:dns-sd)
  (:export #:main))
