;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

(def (function e) make-projection/common-lisp->form ()
  (sequential
    (recursive (make-projection/common-lisp->lisp-form))
    (recursive (make-projection/lisp-form->form))))

(def (function e) make-projection/t->common-lisp ()
  (recursive
    (type-dispatching
      (test/check (make-projection/test/check->common-lisp/application))
      (common-lisp/variable-reference (preserving))
      (common-lisp/function-reference (preserving))
      (t (copying)))))

(def (function e) make-projection/t->form ()
  (sequential
    (make-projection/t->common-lisp)
    (make-projection/common-lisp->form)))
