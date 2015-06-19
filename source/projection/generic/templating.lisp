;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def projection templating ()
  ((template :type document)))

;;;;;;
;;; IO map

(def iomap iomap/templating ()
  ((slot-value-iomaps :type hash-table)))

;;;;;;
;;; Construction

(def function make-projection/templating (template)
  (make-projection 'templating :template template))

;;;;;;
;;; Construction

(def macro templating (&body template)
  `(make-projection/templating ,(first template)))

;;;;;;
;;; Forward mapper

(def function forward-mapper/templating (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap)))
    ;; TODO: do forward mapping based on the template
    (labels ((recurse (instance)
               (etypecase instance
                 ((or number string symbol pathname function sb-sys:system-area-pointer))
                 (document/sequence
                     (iter (for element :in-sequence instance)
                           (recurse element)))
                 (sequence
                  (iter (for element :in-sequence instance)
                        (recurse element)))
                 (template/evaluate-form
                     )
                 (template/recurse-slot
                     )
                 (standard-object
                  (bind ((class (class-of instance))
                         (copy (allocate-instance class))
                         (slots (class-slots class)))
                    (iter (for slot :in slots)
                          (when (slot-boundp-using-class class instance slot)
                            (recurse (slot-value-using-class class instance slot))))
                    copy)))))
      (recurse (template-of projection)))
    (pattern-case reference
      (((the ?output-type (printer-output (the ?input-type document) ?projection ?recursion)) . ?rest)
       (when (and (eq projection ?projection) (eq recursion ?recursion)
                  (eq ?input-type (form-type (input-of printer-iomap)))
                  #+nil
                  (eq ?output-type (form-type (output-of printer-iomap))))
         ?rest)))))

;;;;;;
;;; Backward mapper

(def function backward-mapper/templating (printer-iomap reference)
  (bind ((projection (projection-of printer-iomap))
         (recursion (recursion-of printer-iomap))
         (printer-input (input-of printer-iomap)))
    (iter (for template-element :initially (template-of projection) :then (eval `(bind ((document (quote ,template-element))) ,(car reference-element))))
          (for reference-element :on reference)
          (typecase template-element
            (text/text
                (pattern-case reference-element
                  (((the text/text (text/subseq (the text/text document) ?start-index ?end-index)))
                   (bind ((string-content (content-of (first-elt (elements-of template-element)))))
                     (typecase string-content
                       (template/recurse-slot
                           (bind ((slot-name (name-of string-content))
                                  (slot-value (slot-value printer-input slot-name))
                                  (slot-type (form-type slot-value))
                                  (slot-reader (find-slot-reader (class-of printer-input) (find-slot (class-of printer-input) slot-name))))
                             (return-from backward-mapper/templating `((the ,slot-type (,slot-reader (the ,(form-type printer-input) document)))
                                                                       (the ,slot-type (subseq (the ,slot-type document) ,?start-index ,?end-index)))))))))))
            (template/recurse-slot
                (bind ((slot-name (name-of template-element))
                       (slot-value (slot-value printer-input slot-name))
                       (slot-iomap (gethash slot-name (slot-value-iomaps-of printer-iomap)))
                       (slot-reader (find-slot-reader (class-of printer-input) (find-slot (class-of printer-input) slot-name))))
                  (return-from backward-mapper/templating
                    (if (typep slot-value 'computed-ll)
                        (bind ((index (last-elt (last-elt (car reference-element))))
                               (slot-value-element (elt slot-value index)))
                          (values `((the ,(form-type slot-value) (,slot-reader (the ,(form-type printer-input) document)))
                                    (the ,(form-type slot-value-element) (elt (the ,(form-type slot-value) document) ,index)))
                                  (cdr reference-element)
                                  (elt slot-iomap index)))
                        (values `((the ,(form-type slot-value) (,slot-reader (the ,(form-type printer-input) document))))
                                (cdr reference-element)
                                slot-iomap)))))))
    (pattern-case reference
      (?a
       (append `((the ,(form-type (output-of printer-iomap)) (printer-output (the ,(form-type (input-of printer-iomap)) document) ,projection ,recursion))) reference)))))

;;;;;;
;;; Printer

(def printer templating (projection recursion input input-reference)
  (bind ((output-selection (as (print-selection (make-iomap 'iomap/templating
                                                            :projection projection :recursion recursion
                                                            :input input :input-reference input-reference)
                                                (selection-of input)
                                                'forward-mapper/templating)))
         (slot-value-iomaps (make-hash-table))
         (output (as (bind ((output (labels ((recurse (instance)
                                               (etypecase instance
                                                 ((or number string symbol pathname function sb-sys:system-area-pointer)
                                                  instance)
                                                 (document/sequence
                                                     (make-document/sequence (iter (for element :in-sequence instance)
                                                                                   (collect (recurse element)))
                                                                             :selection (recurse (selection-of instance))))
                                                 (sequence
                                                  (coerce (iter (for element :in-sequence instance)
                                                                (collect (recurse element)))
                                                          (type-of instance)))
                                                 (template/evaluate-form
                                                     (bind ((form (form-of instance)))
                                                       (eval `(bind ((document ,input)) ,form))))
                                                 (template/recurse-slot
                                                     (bind ((slot-name (name-of instance))
                                                            (value (slot-value input slot-name)))
                                                       (if (typep value '(or list computed-ll))
                                                           (bind ((slot-value-iomap (iter (for index :from 0)
                                                                                          (for element :in-sequence value)
                                                                                          (collect (recurse-printer recursion element nil)))))
                                                             (setf (gethash slot-name slot-value-iomaps) slot-value-iomap)
                                                             (mapcar 'output-of slot-value-iomap))
                                                           (bind ((slot-value-iomap (recurse-printer recursion value nil)))
                                                             (setf (gethash slot-name slot-value-iomaps) slot-value-iomap)
                                                             (output-of slot-value-iomap)))))
                                                 (standard-object
                                                  (bind ((class (class-of instance))
                                                         (copy (allocate-instance class))
                                                         (slots (class-slots class)))
                                                    (iter (for slot :in slots)
                                                          (when (slot-boundp-using-class class instance slot)
                                                            (setf (slot-value-using-class class copy slot) (recurse (slot-value-using-class class instance slot)))))
                                                    copy)))))
                                      (recurse (template-of projection)))))
                       ;; KLUDGE: breaks computed
                       #+nil
                       (set-selection output (va output-selection))
                       output))))
    (make-iomap 'iomap/templating
                :projection projection :recursion recursion
                :input input :input-reference input-reference :output output
                :slot-value-iomaps slot-value-iomaps)))

;;;;;;
;;; Reader

(def reader templating (projection recursion input printer-iomap)
  (declare (ignore projection))
  (merge-commands (command/read-selection recursion input printer-iomap 'forward-mapper/templating 'backward-mapper/templating)
                  (command/read-backward recursion input printer-iomap 'backward-mapper/templating nil)
                  (make-command/nothing (gesture-of input))))
