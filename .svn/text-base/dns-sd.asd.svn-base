#| DNS-SD, a Common Lisp library for Zeroconf service discovery. |#

(in-package #:cl-user)
(defpackage #:cl-dns-sd-system (:use #:asdf #:cl))
(in-package #:cl-dns-sd-system)

(defsystem dns-sd
  :name "DNS-SD"
  :author "John Wiseman <jjwiseman@yahoo.com>"
  :maintainer "Sebastián González <s.gonzalez@uclouvain.be>"
  :version "2008-02"
  :licence "MIT"
  :depends-on (cffi s-sysdeps)
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "packages")
     (:file "sysdeps")
     (:file "mdns-ffi")
     (:file "conditions")
     (:file "callbacks")
     (:file "mdns")))))
