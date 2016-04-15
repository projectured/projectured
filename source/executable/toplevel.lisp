;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Toplevel

(def constant +version+ "1.0")

(def constant +command-line-options+
  '((("help" #\h)
     :type boolean
     :optional #t
     :documentation "Print usage help and exit.")
    (("version" #\v)
     :type boolean
     :optional #t
     :documentation "Print version information and exit.")
    (("verbose" #\Space)
     :type boolean
     :optional #t
     :documentation "Provide more information about what's happening.")
    (("quiet" #\q)
     :type boolean
     :optional #t
     :documentation "Do not write anything to standard output.")
    (("disable-debugger" #\Space)
     :type boolean
     :optional #t
     :documentation "Disable the debugger, so that in case of unhandled toplevel errors the process quits.")
    (("swank-port" #\Space)
     :type integer
     :initial-value 4005
     :documentation "The port is used to connect to the running program with SLIME.")
    (("measure-reader" #\Space)
     :type boolean
     :optional #t
     :initial-value #f
     :documentation "Measure reader performance and print results to standard output.")
    (("measure-evaluator" #\Space)
     :type boolean
     :optional #t
     :initial-value #f
     :documentation "Measure evaluator performance and print results to standard output.")
    (("measure-printer" #\Space)
     :type boolean
     :optional #t
     :initial-value #f
     :documentation "Measure printer performance and print results to standard output.")))

(def with-macro with-save-core-and-die-restart ()
  (restart-case
      (-body-)
    #+sbcl
    (save-core-and-die ()
      :report "Save image to /tmp/sbcl.core and die"
      (mapcar
       (lambda (thread)
         (unless (eq thread sb-thread:*current-thread*)
           (sb-thread:terminate-thread thread)))
       (sb-thread:list-all-threads))
      (sb-ext:save-lisp-and-die "/tmp/sbcl.core"))))

(def with-macro with-standard-toplevel-restarts ()
  (restart-case
      (with-save-core-and-die-restart
        (-body-))
    (abort nil
      :report (lambda (stream)
                (format stream "Give up starting the image and quit the VM process with exit code 2"))
      (quit 2))))

(def function process-help-command-line-argument (arguments options)
  (when (getf arguments :help)
    (format *standard-output* "Usage: projectured [option]... [filename]")
    (bind ((extensions (sort (mapcar (compose 'string-downcase 'symbol-name)
                                     (hash-table-keys (hu.dwim.def::entries-of (find-namespace 'loader))))
                             'string<)))
      (format *standard-output* "~%Supported file extensions: ~{~A~^, ~}." extensions))
    (format *standard-output* "~%Options:")
    (command-line-arguments:show-option-help options)
    (format *standard-output* "You can read more about ProjecturEd at http://projectured.org.~%")
    (force-output)
    (quit +process-return-code/no-error+)))

(def function process-version-command-line-argument (arguments version-string)
  (when (getf arguments :version)
    (format *standard-output* "Version: ~A~%" version-string)
    (force-output)
    (quit +process-return-code/no-error+)))

(def function process-quiet-command-line-argument (arguments)
  (when (getf arguments :quiet)
    (setf *standard-output* (make-instance 'broadcast-stream))))

(def function process-measure-command-line-argument (arguments argument)
  (when (getf arguments argument)
    :measure))

(def function make-default-editor (documents)
  (bind ((document (make-default-document (first documents)))
         (projection (make-default-projection))
         (backend (projectured.sdl::make-sdl-backend))
         (devices (make-default-devices)))
    (make-editor document projection backend devices)))

(def function executable-toplevel ()
  (with-standard-toplevel-restarts
    (bind (((:values arguments filenames) (command-line-arguments:process-command-line-options +command-line-options+ (rest sb-ext:*posix-argv*))))
      (process-help-command-line-argument arguments +command-line-options+)
      (process-version-command-line-argument arguments +version+)
      (process-quiet-command-line-argument arguments)
      (editor.debug "Parsed command line arguments are: ~S" arguments)
      (bind ((documents (mapcar 'make-file-document filenames))
             (editor (make-default-editor (first documents)))
             (measure-reader (process-measure-command-line-argument arguments :measure-reader))
             (measure-evaluator (process-measure-command-line-argument arguments :measure-evaluator))
             (measure-printer (process-measure-command-line-argument arguments :measure-printer)))
        (call-read-evaluate-print-loop editor
                                       :measure-reader measure-reader
                                       :measure-evaluator measure-evaluator
                                       :measure-printer measure-printer)))
    +process-return-code/no-error+))
