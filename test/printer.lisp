;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

;;;;;;
;;; Automated printer test suite

(def suite* (test/printer :in test))

(def test test/printer/exhaustive (projection document-mapper selection-mapper)
  (bind ((index 0))
    (ensure-directories-exist #P"/tmp/projectured/")
    (funcall document-mapper
             (lambda (document)
               (funcall selection-mapper document
                        (lambda (selection)
                          (bind ((editor (make-instance 'editor/sdl
                                                        :devices (list (make-device/file/sdl (pathname (format nil "/tmp/projectured/~A.bmp" index))))
                                                        :event-queue (make-instance 'event-queue :events nil)
                                                        :gesture-queue (make-instance 'gesture-queue :gestures nil))))
                            (set-selection document selection)
                            (finishes (run-read-evaluate-print-loop editor document projection))
                            (incf index))))))))

(def test test/printer/text->graphics (count offset)
  (test/printer/exhaustive (text->graphics)
                           (lambda (function)
                             (iter (for document-index :from 0 :below (expt 2 count))
                                   (for document = (text/make-text (ll (iter (for element-index :from 0 :below count)
                                                                             (for element :initially document-index :then (floor (/ element 2)))
                                                                             (collect (if (zerop (mod element 2))
                                                                                          (text/string "x")
                                                                                          (text/newline))))
                                                                       offset)))
                                   (funcall function document)))
                           (lambda (document function)
                             (bind ((length (text/length document)))
                               (iter (for index :from 0 :to length)
                                     (for selection = `((the text/text (text/subseq (the text/text document) ,(- index offset) ,(- index offset)))))
                                     (funcall function selection))
                               (iter (for start :from 0 :to length)
                                     (iter (for end :from (1+ start) :to length)
                                           (for selection = `((the text/text (text/subbox (the text/text document) ,(- start offset) ,(- end offset)))))
                                           (funcall function selection)))))))
