;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;
;;; Editor API
;;;
;;; An editor is a way of changing the state of a document.

(def (generic e) make-editor (&key width height)
  (:documentation "Returns a new editor object. Purely functional.")
  (:method (&key &allow-other-keys)
    (error "The default ~S method was called. Did you forget to load a backend? Try e.g. (asdf:load-system :projectured.sdl)."
           'make-editor)))

(def (generic e) read-from-devices (editor document projection)
  (:documentation "Reads an operation from the input devices of EDITOR in the context of DOCUMENT using PROJECTION. Does not return until it has successfully read an operation. Has side effects on the state of the input devices, the list of events, gestures and operations that has been read so far by EDITOR."))

(def (generic e) print-to-devices (editor document projection)
  (:documentation "Prints DOCUMENT to the output devices of EDITOR using PROJECTION. Has side effects on the state of the output devices of EDITOR."))

(def (generic e) run-read-evaluate-print-loop (editor document projection)
  (:documentation "Runs read-evaluate-print loop of EDITOR on DOCUMENT using PROJECTION. The loop first prints DOCUMENT to the output devices of EDITOR. Next it reads an operation from the input devices of EDITOR. Finally it evaluates the result of read operation and starts over. Has side effects continuously on the state of EDITOR and the content of DOCUMENT while running."))

;;;;;;
;;; Editor classes

(def class* editor ()
  ((devices :type sequence)
   (reader-iomap :type iomap)
   (printer-iomap :type iomap)
   ;; TODO: merge these into reader-iomap
   (event-queue :type event-queue)
   (gesture-queue :type gesture-queue)))

;;;;;;
;;; Editor API implementation

(def method run-read-evaluate-print-loop ((editor editor) document projection)
  (catch :quit-editor
    (iter (print-to-devices editor document projection)
          (funcall (read-from-devices editor document projection)))))

(def method read-from-devices ((editor editor) document projection)
  (bind ((event-queue (event-queue-of editor))
         (gesture-queue (gesture-queue-of editor)))
    (iter (when-bind event (read-event (remove-if-not 'input-device? (devices-of editor)))
            (push event (events-of event-queue))
            (when-bind gesture (read-gesture event-queue)
              (push gesture (gestures-of gesture-queue))
              (when-bind operation (operation-of (apply-reader (make-command gesture nil :domain "Default" :description "Does nothing") projection (printer-iomap-of editor)))
                (return (lambda ()
                          (editor.debug "Redoing ~A" operation)
                          (redo-operation operation)))))))))

(def method print-to-devices ((editor editor) document projection)
  (bind ((printer-iomap (setf (printer-iomap-of editor) (apply-printer document projection))
                        #+nil
                        (if (slot-boundp editor 'printer-iomap)
                            (printer-iomap-of editor)
                            (setf (printer-iomap-of editor) (time (apply-printer document projection))))))
    (iter (for device :in-sequence (remove-if-not 'output-device? (devices-of editor)))
          (print-to-device (output-of printer-iomap) device))))
