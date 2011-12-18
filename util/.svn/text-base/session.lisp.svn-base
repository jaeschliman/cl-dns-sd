#| This file is part of DNS-SD, a Common Lisp library for Zeroconf service discovery. |#

(in-package #:cl-user)

(require 'asdf)

;; Load DNS-SD
(asdf:oos 'asdf:load-op 'dns-sd)

;; might prove useful
(setq dns-sd::*debug-logging-p* t)

;; Run unit tests
(asdf:oos 'asdf:load-op 'dns-sd-tests)
(it.bese.fiveam:run! 'dns-sd-tests:main)
