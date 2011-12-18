#| DNS-SD, a Common Lisp library for Zeroconf service discovery. |#

(in-package #:cl-user)
(defpackage #:cl-dns-sd-system (:use #:asdf #:cl))
(in-package #:cl-dns-sd-system)

(defsystem dns-sd-tests
  :depends-on (dns-sd fiveam)
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "packages")
     (:file "testsuite")
     (:file "basic")))))
