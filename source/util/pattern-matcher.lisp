;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Binding environment

(def constant +failed-match+ nil)

(def constant +no-bindings+ (if (boundp 'no-bindings)
                                (symbol-value 'no-bindings)
                                '((t . t))))

(def function match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t +failed-match+))))

(def function make-binding (var val) (cons var val))

(def function binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(def function binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(def function get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(def function lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(def function extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy +no-bindings+
        (if (equal bindings +no-bindings+)
            nil
            bindings)))

(def function pattern-variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(def function pattern-anything-p (x)
  (eq x '?))

;;;;;;
;;; Matcher (PAIPROLOG matcher + objects)

(def (function e) pattern-match (pattern input &optional (bindings +no-bindings+))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings +failed-match+) +failed-match+)
        ((pattern-variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-matcher pattern input bindings))
        ((single-pattern-p pattern)     ; ***
         (single-matcher pattern input bindings)) ; ***
        ((object-pattern-p pattern)
         (object-matcher pattern input bindings))
        ((and (consp-cc pattern) (consp-cc input))
         (bind ((pattern-cdr (cdr-cc pattern))
                (bindings (pattern-match (car-cc pattern) (car-cc input) bindings)))
           (if (pattern-anything-p pattern-cdr)
               bindings
               (pattern-match pattern-cdr (cdr-cc input) bindings))))
        (t +failed-match+)))

(def (macro e) pattern-case (expr &body clauses)
  (with-unique-names (expr-var)
    `(bind ((,expr-var ,expr))
      (acond
       ,@(mapcar
          (lambda (clause)
            (bind ((pattern-vars (collect-pattern-variables (car clause))))
              `((pattern-match ',(car clause) ,expr-var)
                (let ,(mapcar (lambda (patter-var) `(,patter-var (binding-val (get-binding ',patter-var it)))) pattern-vars)
                  (declare (ignorable ,@pattern-vars))
                  ,@(cdr clause)))))
          clauses)))))

(def function collect-pattern-variables (syntax &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (labels ((recurse ()
             (typecase syntax
               (standard-object
                (collect-slots (mapcar 'slot-definition-name (class-slots (class-of syntax)))))
               (cons
                (collect-pattern-variables
                 (car syntax)
                 (collect-pattern-variables (cdr syntax) found-so-far)))
               (otherwise
                found-so-far)))
           (collect-slots (slots)
             (cond
               ((null slots) found-so-far)
               ((slot-boundp syntax (first slots))
                (collect-pattern-variables (slot-value syntax (first slots))
                                           (collect-slots (rest slots))))
               (t (collect-slots (rest slots))))))
    (if (pattern-variable-p syntax)
        (adjoin syntax found-so-far)
        (recurse))))

(setf (get '?is  'single-match) 'match-is)
(setf (get '?or  'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)
(setf (get '?*  'segment-match) 'segment-match)
(setf (get '?+  'segment-match) 'segment-match+)
(setf (get '??  'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)
(export '(?is ?or ?and ?not ?* ?+ ?? ?if))

(def function single-pattern-p (pattern)
  "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  (and (consp pattern)
       (single-match-fn (first pattern))))

(def function single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (single-match-fn (first pattern))
           (rest pattern) input bindings))

(def function single-match-fn (x)
  "Get the single-match function for x,
  if it is a symbol that has one."
  (when (symbolp x) (get x 'single-match)))

(def function segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (segment-match-fn (first (first pattern)))
           pattern input bindings))

(def function segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  (and (consp pattern) (consp (first pattern))
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(def function segment-match-fn (x)
  "Get the segment-match function for x,
  if it is a symbol that has one."
  (when (symbolp x) (get x 'segment-match)))

(def function segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        (let ((pos (first-match-pos (first pat) input start)))
          (if (null pos)
              +failed-match+
              (let ((b2 (pattern-match
                          pat (subseq input pos)
                          (match-variable var (subseq input 0 pos)
                                          bindings))))
                ;; If this match failed, try another longer one
                (if (eq b2 +failed-match+)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

(def function first-match-pos (pat1 input start)
  "Find the first position that pat1 could possibly match input,
  starting at position start.  If pat1 is non-constant, then just
  return start."
  (cond ((and (atom pat1) (not (pattern-variable-p pat1)))
         (position pat1 input :start start :test #'equal))
        ((<= start (length input)) start) ;*** fix, rjf 10/1/92 (was <)
        (t nil)))

(def function segment-match+ (pattern input bindings)
  "Match one or more elements of input."
  (segment-match pattern input bindings 1))

(def function segment-match? (pattern input bindings)
  "Match zero or one element of input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (or (pattern-match (cons var pat) input bindings)
        (pattern-match pat input bindings))))

(def function object-pattern-p (pattern)
  (typep pattern 'standard-object))

(def function object-matcher (pattern input bindings)
  (labels ((slot-matcher (slots bindings)
             (cond
               ((eq bindings +failed-match+) +failed-match+)
               ((null slots) bindings)
               ((and (slot-boundp pattern (first slots))
                     (slot-boundp input (first slots)))
                (slot-matcher (rest slots)
                              (pattern-match (slot-value pattern (first slots))
                                             (slot-value input (first slots))
                                             bindings)))
               ((slot-boundp pattern (first slots)) +failed-match+)
               (t (slot-matcher (rest slots) bindings)))))
    (if (or (eq bindings +failed-match+) (not (typep input (class-of pattern))))
        +failed-match+
        (bind ((slots (mapcar 'slot-definition-name (class-slots (class-of pattern)))))
          (slot-matcher slots bindings)))))

(def function match-if (pattern input bindings)
  "Test an arbitrary expression involving variables.
  The pattern looks like ((?if code) . rest)."
  ;; *** fix, rjf 10/1/92 (used to eval binding values)
  (and (progv (mapcar #'car bindings)
           (mapcar #'cdr bindings)
         (eval `(locally (declare (special ,@(mapcar #'car bindings)))
                 ,(second (first pattern)))))
       (pattern-match (rest pattern) input bindings)))

(def function match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
  (let* ((var (first var-and-pred))
         (pred (second var-and-pred))
         (new-bindings (pattern-match var input bindings)))
    (if (or (eq new-bindings +failed-match+)
            (not (funcall pred input)))
        +failed-match+
        new-bindings)))

(def function match-and (patterns input bindings)
  "Succeed if all the patterns match the input."
  (cond ((eq bindings +failed-match+) +failed-match+)
        ((null patterns) bindings)
        (t (match-and (rest patterns) input
                      (pattern-match (first patterns) input
                                     bindings)))))

(def function match-or (patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (if (null patterns)
      +failed-match+
      (let ((new-bindings (pattern-match (first patterns)
                                         input bindings)))
        (if (eq new-bindings +failed-match+)
            (match-or (rest patterns) input bindings)
            new-bindings))))

(def function match-not (patterns input bindings)
  "Succeed if none of the patterns match the input.
  This will never bind any variables."
  (if (match-or patterns input bindings)
      +failed-match+
      bindings))
