;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Automatic navigation test suite

(def suite* (test/navigation :in test))

(def test test/navigation/sequential (&key demo instance document projection (log-level +warn+) navigation-count navigation-time)
  (with-demo (:name demo :instance instance)
    (with-logger-level (editor log-level)
      (with-logger-level (test log-level)
        (iter (with start-time = (get-internal-run-time))
              (with right-arrow-gesture = (make-instance 'gesture/keyboard/key-press
                                                         :key :sdl-key-right
                                                         :character nil
                                                         :modifiers nil
                                                         :location (mouse-position)))
              (with control-home-gesture = (make-instance 'gesture/keyboard/key-press
                                                          :key :sdl-key-home
                                                          :character nil
                                                          :modifiers (list :control)
                                                          :location (mouse-position)))
              (for count :from 0)
              (initially
               (run-operation (operation-of (apply-reader (make-command/nothing control-home-gesture) projection (apply-printer document projection)))))
              (for printer-output = (apply-printer document projection))
              (labels ((recurse (instance)
                         (etypecase instance
                           ((or graphics/text graphics/image graphics/line))
                           (graphics/canvas (foreach #'recurse (elements-of instance)))
                           (graphics/viewport (recurse (content-of instance))))))
                (recurse (output-of printer-output)))
              (for command = (apply-reader (make-command/nothing right-arrow-gesture) projection printer-output))
              (while (operation-of command))
              (run-operation (operation-of command))
              (finally
               (bind ((total-time (coerce (/ (- (get-internal-run-time) start-time) internal-time-units-per-second) 'float))
                      (average-time (unless (zerop count) (* (/ total-time count) 1000))))
                 (when navigation-count
                   (is (= navigation-count count)))
                 (when navigation-time
                   (is (< average-time navigation-time)))
                 (test.info "Completed processing ~A navigations in ~A seconds (average ~A ms)" count total-time average-time)
                 (return (values count average-time)))))))))

(def test test/navigation/exhaustive (&key demo instance document projection (log-level +warn+) navigation-count navigation-time)
  (with-demo (:name demo :instance instance)
    (with-logger-level (editor log-level)
      (with-logger-level (test log-level)
        (iter (with start-time = (get-internal-run-time))
              (with help-gesture = (make-instance 'gesture/keyboard/key-press
                                                  :key :sdl-key-h
                                                  :character #\h
                                                  :modifiers (list :control)
                                                  :location (mouse-position)))
              (with processed-gestures = nil)
              (with processed-selections = nil)
              (with remaining-selections = (list (selection-of document)))
              (while remaining-selections)
              (for count :from 0)
              (for selected-selection = (pop remaining-selections))
              (test.debug "Testing selection: ~A" selected-selection)
              (set-selection document selected-selection)
              (for printer-iomap = (apply-printer document projection))
              (for show-help-command = (apply-reader (make-command/nothing help-gesture) projection printer-iomap))
              (push selected-selection processed-selections)
              (iter (for command :in-sequence (commands-of (operation-of show-help-command)))
                    (for gesture = (gesture-of command))
                    (for operation = (operation-of command))
                    (test.debug "Checking gesture ~A resulting in ~A for new selection" gesture operation)
                    (when (and (typep gesture 'gesture/keyboard/key-press)
                               (typep operation 'operation/replace-selection))
                      (pushnew gesture processed-gestures :test 'gesture=)
                      (bind ((new-selection (selection-of operation)))
                        (when (and new-selection
                                   (not (member new-selection remaining-selections :test 'equal))
                                   (not (member new-selection processed-selections :test 'equal)))
                          (test.debug "Adding new selection ~A produced by ~A" new-selection gesture)
                          (push new-selection remaining-selections)))))
              (test.debug "Remaining selections to test: ~A" (length remaining-selections))
              (finally
               (bind ((total-time (coerce (/ (- (get-internal-run-time) start-time) internal-time-units-per-second) 'float))
                      (average-time (unless (zerop count) (* (/ total-time count) 1000))))
                 (setf processed-gestures (sort processed-gestures 'string< :key 'gesture/describe))
                 (setf processed-selections (sort processed-selections 'string< :key 'write-to-string))
                 (iter (for gesture :in processed-gestures)
                       (test.info "Processed gesture: ~A" gesture))
                 (iter (for selection :in processed-selections)
                       (test.info "Procecssed selection: ~A" selection))
                 (when navigation-count
                   (is (= navigation-count count)))
                 (when navigation-time
                   (is (< average-time navigation-time)))
                 (test.info "Completed processing ~A gestures and ~A selections in ~A seconds (average ~A ms)" (length processed-gestures) (length processed-selections) total-time average-time)
                 (return (values (length processed-gestures) (length processed-selections) average-time)))))))))

(def test test/navigation/demo ()
  (test/navigation/sequential :demo 'css :navigation-count 47 :navigation-time 100)
  (test/navigation/sequential :demo 'common-lisp :navigation-count 112 :navigation-time 100)
  (test/navigation/sequential :demo 'xml :navigation-count 188 :navigation-time 100)
  (test/navigation/sequential :demo 'json :navigation-count 193 :navigation-time 100)
  (test/navigation/sequential :demo 'book :navigation-count 1118 :navigation-time 100)
  #+nil
  (test/navigation/sequential :demo 'javascript :navigation-count 112 :navigation-time 100))
