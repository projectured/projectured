;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

(def special-variable *check-ll* nil)

;; TODO: rename to coerce-ll?
(def function ll (sequence &optional (index 0))
  (if (typep sequence 'computed-ll)
      sequence
      (unless (emptyp sequence)
        (labels ((make (index previous next)
                   (make-computed-ll (as (elt sequence index))
                                     (as (when (> index 0)
                                           (or previous (make (1- index) nil -self-))))
                                     (as (when (< index (1- (length sequence)))
                                           (or next (make (1+ index) -self- nil)))))))
          (elt-ll (make 0 nil nil) index)))))

(def function make-ll (length &key (initial-element nil initial-element?) initial-contents)
  (if initial-element?
      (ll (make-list length :initial-element initial-element))
      (list-ll initial-contents)))

(def function list-ll (&rest args)
  (ll args))

(def method sb-sequence:make-sequence-like ((sequence computed-ll) length &key (initial-element nil initial-element?) initial-contents)
  (ll (if initial-element?
          (make-list length :initial-element initial-element)
          (concatenate 'list initial-contents (make-list (- length (length initial-contents)))))))

;; TODO: rename to last-elt-ll?
(def function last-element (sequence)
  (iter (for e :initially sequence :then (next-element-of e))
        (for previous-e :previous e)
        (while e)
        (finally (return previous-e))))

;; TODO: rename to first-elt-ll?
(def function first-element (sequence)
  (iter (for e :initially sequence :then (previous-element-of e))
        (for previous-e :previous e)
        (while e)
        (finally (return previous-e))))

(def function check-ll (sequence)
  (when *check-ll*
    (assert (eq (first-element sequence) (first-element (last-element sequence))))
    (assert (eq (last-element (first-element sequence)) (last-element sequence)))
    (assert (equal (iter (for element :initially (first-element sequence) :then (next-element-of element))
                         (while element)
                         (collect element))
                   (reverse (iter (for element :initially (last-element sequence) :then (previous-element-of element))
                                  (while element)
                                  (collect element)))))))

(def function map-ll (sequence function)
  (check-ll sequence)
  (when sequence
    (labels ((recurse (element previous-element next-element)
               (make-computed-ll (as (funcall function (value-of element)))
                                 (as (or previous-element
                                         (awhen (previous-element-of element)
                                           (recurse it nil -self-))))
                                 (as (or next-element
                                         (awhen (next-element-of element)
                                           (recurse it -self- nil)))))))
      (recurse sequence nil nil))))

(def function map-ll* (sequence function)
  (check-ll sequence)
  (when sequence
    (labels ((recurse (element previous-element next-element index)
               (make-computed-ll (as (funcall function element index))
                                 (as (or previous-element
                                         (awhen (previous-element-of element)
                                           (recurse it nil -self- (1- index)))))
                                 (as (or next-element
                                         (awhen (next-element-of element)
                                           (recurse it -self- nil (1+ index))))))))
      (recurse sequence nil nil 0))))


(def function subseq-ll (sequence start end)
  (check-ll sequence)
  (if (zerop start)
      (unless (zerop end)
        (make-computed-ll (as (value-of sequence))
                          ;; TODO:
                          (as nil)
                          (as (subseq-ll (next-element-of sequence) 0 (1- end)))))
      (subseq-ll (next-element-of sequence) (1- start) (1- end))))

(def function concatenate-ll (index &rest sequences)
  (append-ll (ll (mapcar 'll (remove-if 'null sequences)) index)))

(def function append-ll (sequences)
  (when sequences
    (labels ((recurse (sequence sequence-element previous-sequence-element next-sequence-element previous-element next-element)
               (check-ll sequence)
               (when sequence
                 (make-computed-ll (as (value-of sequence))
                                   (as (or previous-element
                                           (aif (previous-element-of sequence)
                                                (recurse it sequence-element previous-sequence-element next-sequence-element nil -self-)
                                                (when previous-sequence-element
                                                  (recurse (last-element (value-of previous-sequence-element)) previous-sequence-element
                                                           (previous-element-of previous-sequence-element) sequence-element
                                                           nil -self-)))))
                                   (as (or next-element
                                           (aif (next-element-of sequence)
                                                (recurse it sequence-element previous-sequence-element next-sequence-element -self- nil)
                                                (when next-sequence-element
                                                  (recurse (first-element (value-of next-sequence-element)) next-sequence-element
                                                           sequence-element (next-element-of next-sequence-element)
                                                           -self- nil)))))))))
      (recurse (value-of sequences) sequences (previous-element-of sequences) (next-element-of sequences) nil nil))))

(def function separate-elements-ll (sequence separator)
  (check-ll sequence)
  (when sequence
    (labels ((recurse (sequence previous-element next-element)
               (make-computed-ll (as (value-of sequence))
                                 (as (when (previous-element-of sequence)
                                       (make-computed-ll (as separator)
                                                         (as (recurse (previous-element-of sequence) nil -self-))
                                                         (as next-element))))
                                 (as (when (next-element-of sequence)
                                       (make-computed-ll (as separator)
                                                         (as previous-element)
                                                         (as (recurse (next-element-of sequence) -self- nil))))))))
      (recurse sequence nil nil))))
