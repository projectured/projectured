;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(defsystem :projectured.executable
  :description "Generic purpose projectional editor."
  :author "Levente Mészáros"
  :licence "BSD"

  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :entry-point "projectured::executable-toplevel"
  ;; TODO from: https://gitlab.common-lisp.net/asdf/asdf/merge_requests/34#note_2181
  ;; "Note though that I dislike :build-pathname so for more
  ;; future-proofing, I recommend not using it, and instead copying
  ;; from the (output-files 'program-op "my/program") to the
  ;; destination, or explaining how otherwise to do the
  ;; :build-pathname bit right and why it's right."
  :build-pathname "projectured" ;; the base name of the executable (unless it's overridden, e.g. in bin/build.sh)
  :package-name :projectured
  :depends-on (:command-line-arguments
               :projectured.sdl)
  :components ((:module "source"
                :components ((:module "executable"
                              :components ((:file "document")
                                           (:file "projection")
                                           (:file "toplevel" :depends-on ("document" "projection"))))))))
