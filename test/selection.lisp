;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Selection

(def suite* (test/selection :in test))

(def test test/selection/navigation (document projection)
  (iter (with help-gesture = (make-instance 'gesture/keyboard/key-press
                                            :key :sdl-key-h
                                            :modifiers (list :control)
                                            :location (mouse-position)))
        (with processed-gestures = nil)
        (with processed-selections = nil)
        (with remaining-selections = (list (selection-of document)))
        (while remaining-selections)
        (for selected-selection = (pop remaining-selections))
        (test.debug "Testing selection: ~A" selected-selection)
        (set-selection document selected-selection)
        (for printer-iomap = (finishes (apply-printer document projection)))
        (for show-help-command = (finishes (apply-reader (make-command/nothing help-gesture) projection printer-iomap)))
        (push selected-selection processed-selections)
        (iter (for command :in-sequence (commands-of (operation-of show-help-command)))
              (for gesture = (gesture-of command))
              (for operation = (operation-of command))
              (when (and (typep gesture 'gesture/keyboard/key-press)
                         (typep operation 'operation/replace-selection))
                (pushnew gesture processed-gestures :test 'gesture=)
                (bind ((new-selection (selection-of operation)))
                  (when (and new-selection
                             (not (member new-selection remaining-selections :test 'equal))
                             (not (member new-selection processed-selections :test 'equal)))
                    (test.debug "Processing gesture ~A, adding new selection: ~A" (gesture/describe gesture) new-selection)
                    (push new-selection remaining-selections)))))
        (test.debug "Remaining selections to test: ~A" (length remaining-selections))
        (finally
         (iter (for gesture :in processed-gestures)
               (test.info "Processed gesture: ~A" (gesture/describe gesture)))
         (iter (for selection :in processed-selections)
               (test.info "Procecssed selection: ~A" selection))
         (test.info "Completed processing ~A gestures and ~A selections" (length processed-gestures) (length processed-selections)))))

(def test test/selection/text ()
  (test/selection/navigation (make-test-document/text/empty) (make-test-projection/text->graphics))
  (test/selection/navigation (make-test-document/text) (make-test-projection/text->graphics))
  (test/selection/navigation (make-test-document/text/ll 10 5) (make-test-projection/text->graphics)))

(def test test/selection/tree ()
  (test/selection/navigation (make-test-document/tree) (make-test-projection/tree->graphics)))
