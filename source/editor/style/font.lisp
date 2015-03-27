;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document style/font (style/base)
  ((filename :type string)
   (size :type number)
   (raw :type t)))

;;;;;;
;;; Construction

(def function make-style/font (filename size)
  (make-instance 'style/font
                 :filename filename
                 :size size
                 :raw nil))

;;;;;;
;;; API

(def generic measure-text (text font)
  (:documentation "Measures the size of TEXT drawn with FONT."))

(def method hu.dwim.serializer:write-object-slots ((class standard-class) (object style/font) context)
  (bind ((class (class-of object))
         (slots (closer-mop:class-slots class)))
    (hu.dwim.serializer::write-variable-length-positive-integer (length slots) context)
    (dolist (slot slots)
      (unless (eq (ignore-errors (closer-mop:slot-definition-allocation slot)) :class)
        (hu.dwim.serializer::serialize-symbol (closer-mop:slot-definition-name slot) context)
        (if (closer-mop:slot-boundp-using-class class object slot)
            (hu.dwim.serializer::serialize-element (if (eq (slot-definition-name slot) 'raw) nil (closer-mop:slot-value-using-class class object slot)) context)
            (hu.dwim.serializer::write-unsigned-byte-8 hu.dwim.serializer::+unbound-slot-code+ context))))))

;;;;;;
;;; Definer

(def (definer :available-flags "e") font (name (filename size))
  `(def special-variable ,name (make-style/font ,filename ,size)))

;;;;;;
;;; Default

(def font *font/default* ("font/UbuntuMono-R.ttf" 24))

;;;;;;
;;; Inconsolata

(def font *font/inconsolata/regular/18* ("font/Inconsolata.otf" 18))

;;;;;;
;;; Ubuntu monospace

(def font *font/ubuntu/monospace/regular/14* ("font/UbuntuMono-R.ttf" 14))
(def font *font/ubuntu/monospace/italic/14* ("font/UbuntuMono-RI.ttf" 14))
(def font *font/ubuntu/monospace/bold/14* ("font/UbuntuMono-B.ttf" 14))

(def font *font/ubuntu/monospace/regular/16* ("font/UbuntuMono-R.ttf" 16))
(def font *font/ubuntu/monospace/italic/16* ("font/UbuntuMono-RI.ttf" 16))
(def font *font/ubuntu/monospace/bold/16* ("font/UbuntuMono-B.ttf" 16))

(def font *font/ubuntu/monospace/regular/18* ("font/UbuntuMono-R.ttf" 18))
(def font *font/ubuntu/monospace/italic/18* ("font/UbuntuMono-RI.ttf" 18))
(def font *font/ubuntu/monospace/bold/18* ("font/UbuntuMono-B.ttf" 18))

(def font *font/ubuntu/monospace/regular/20* ("font/UbuntuMono-R.ttf" 20))
(def font *font/ubuntu/monospace/italic/20* ("font/UbuntuMono-RI.ttf" 20))
(def font *font/ubuntu/monospace/bold/20* ("font/UbuntuMono-B.ttf" 20))

(def font *font/ubuntu/monospace/regular/22* ("font/UbuntuMono-R.ttf" 22))
(def font *font/ubuntu/monospace/italic/22* ("font/UbuntuMono-RI.ttf" 22))
(def font *font/ubuntu/monospace/bold/22* ("font/UbuntuMono-B.ttf" 22))

(def font *font/ubuntu/monospace/regular/24* ("font/UbuntuMono-R.ttf" 24))
(def font *font/ubuntu/monospace/italic/24* ("font/UbuntuMono-RI.ttf" 24))
(def font *font/ubuntu/monospace/bold/24* ("font/UbuntuMono-B.ttf" 24))

;;;;;;
;;; Ubuntu

(def font *font/ubuntu/regular/14* ("font/Ubuntu-R.ttf" 14))
(def font *font/ubuntu/italic/14* ("font/Ubuntu-RI.ttf" 14))
(def font *font/ubuntu/bold/14* ("font/Ubuntu-B.ttf" 14))

(def font *font/ubuntu/regular/16* ("font/Ubuntu-R.ttf" 16))
(def font *font/ubuntu/italic/16* ("font/Ubuntu-RI.ttf" 16))
(def font *font/ubuntu/bold/16* ("font/Ubuntu-B.ttf" 16))

(def font *font/ubuntu/regular/18* ("font/Ubuntu-R.ttf" 18))
(def font *font/ubuntu/italic/18* ("font/Ubuntu-RI.ttf" 18))
(def font *font/ubuntu/bold/18* ("font/Ubuntu-B.ttf" 18))

(def font *font/ubuntu/regular/20* ("font/Ubuntu-R.ttf" 20))
(def font *font/ubuntu/italic/20* ("font/Ubuntu-RI.ttf" 20))
(def font *font/ubuntu/bold/20* ("font/Ubuntu-B.ttf" 20))

(def font *font/ubuntu/regular/22* ("font/Ubuntu-R.ttf" 22))
(def font *font/ubuntu/italic/22* ("font/Ubuntu-RI.ttf" 22))
(def font *font/ubuntu/bold/22* ("font/Ubuntu-B.ttf" 22))

(def font *font/ubuntu/regular/24* ("font/Ubuntu-R.ttf" 24))
(def font *font/ubuntu/italic/24* ("font/Ubuntu-RI.ttf" 24))
(def font *font/ubuntu/bold/24* ("font/Ubuntu-B.ttf" 24))

(def font *font/ubuntu/regular/36* ("font/Ubuntu-R.ttf" 36))
(def font *font/ubuntu/italic/36* ("font/Ubuntu-RI.ttf" 36))
(def font *font/ubuntu/bold/36* ("font/Ubuntu-B.ttf" 36))

;;;;;;
;;; Liberation sans

(def font *font/liberation/sans/regular/14* ("font/LiberationSans-Regular.ttf" 14))
(def font *font/liberation/sans/italic/14* ("font/LiberationSans-Italic.ttf" 14))
(def font *font/liberation/sans/bold/14* ("font/LiberationSans-Bold.ttf" 14))

(def font *font/liberation/sans/regular/16* ("font/LiberationSans-Regular.ttf" 16))
(def font *font/liberation/sans/italic/16* ("font/LiberationSans-Italic.ttf" 16))
(def font *font/liberation/sans/bold/16* ("font/LiberationSans-Bold.ttf" 16))

(def font *font/liberation/sans/regular/18* ("font/LiberationSans-Regular.ttf" 18))
(def font *font/liberation/sans/italic/18* ("font/LiberationSans-Italic.ttf" 18))
(def font *font/liberation/sans/bold/18* ("font/LiberationSans-Bold.ttf" 18))

(def font *font/liberation/sans/regular/20* ("font/LiberationSans-Regular.ttf" 20))
(def font *font/liberation/sans/italic/20* ("font/LiberationSans-Italic.ttf" 20))
(def font *font/liberation/sans/bold/20* ("font/LiberationSans-Bold.ttf" 20))

(def font *font/liberation/sans/regular/22* ("font/LiberationSans-Regular.ttf" 22))
(def font *font/liberation/sans/italic/22* ("font/LiberationSans-Italic.ttf" 22))
(def font *font/liberation/sans/bold/22* ("font/LiberationSans-Bold.ttf" 22))

(def font *font/liberation/sans/regular/24* ("font/LiberationSans-Regular.ttf" 24))
(def font *font/liberation/sans/italic/24* ("font/LiberationSans-Italic.ttf" 24))
(def font *font/liberation/sans/bold/24* ("font/LiberationSans-Bold.ttf" 24))

(def font *font/liberation/sans/regular/30* ("font/LiberationSans-Regular.ttf" 30))
(def font *font/liberation/sans/italic/30* ("font/LiberationSans-Italic.ttf" 30))
(def font *font/liberation/sans/bold/30* ("font/LiberationSans-Bold.ttf" 30))

(def font *font/liberation/sans/regular/36* ("font/LiberationSans-Regular.ttf" 36))
(def font *font/liberation/sans/italic/36* ("font/LiberationSans-Italic.ttf" 36))
(def font *font/liberation/sans/bold/36* ("font/LiberationSans-Bold.ttf" 36))

;;;;;;
;;; Liberation serif

(def font *font/liberation/serif/regular/14* ("font/LiberationSerif-Regular.ttf" 14))
(def font *font/liberation/serif/italic/14* ("font/LiberationSerif-Italic.ttf" 14))
(def font *font/liberation/serif/bold/14* ("font/LiberationSerif-Bold.ttf" 14))

(def font *font/liberation/serif/regular/16* ("font/LiberationSerif-Regular.ttf" 16))
(def font *font/liberation/serif/italic/16* ("font/LiberationSerif-Italic.ttf" 16))
(def font *font/liberation/serif/bold/16* ("font/LiberationSerif-Bold.ttf" 16))

(def font *font/liberation/serif/regular/18* ("font/LiberationSerif-Regular.ttf" 18))
(def font *font/liberation/serif/italic/18* ("font/LiberationSerif-Italic.ttf" 18))
(def font *font/liberation/serif/bold/18* ("font/LiberationSerif-Bold.ttf" 18))

(def font *font/liberation/serif/regular/20* ("font/LiberationSerif-Regular.ttf" 20))
(def font *font/liberation/serif/italic/20* ("font/LiberationSerif-Italic.ttf" 20))
(def font *font/liberation/serif/bold/20* ("font/LiberationSerif-Bold.ttf" 20))

(def font *font/liberation/serif/regular/22* ("font/LiberationSerif-Regular.ttf" 22))
(def font *font/liberation/serif/italic/22* ("font/LiberationSerif-Italic.ttf" 22))
(def font *font/liberation/serif/bold/22* ("font/LiberationSerif-Bold.ttf" 22))

(def font *font/liberation/serif/regular/24* ("font/LiberationSerif-Regular.ttf" 24))
(def font *font/liberation/serif/italic/24* ("font/LiberationSerif-Italic.ttf" 24))
(def font *font/liberation/serif/bold/24* ("font/LiberationSerif-Bold.ttf" 24))

(def font *font/liberation/serif/regular/30* ("font/LiberationSerif-Regular.ttf" 30))
(def font *font/liberation/serif/italic/30* ("font/LiberationSerif-Italic.ttf" 30))
(def font *font/liberation/serif/bold/30* ("font/LiberationSerif-Bold.ttf" 30))

(def font *font/liberation/serif/regular/36* ("font/LiberationSerif-Regular.ttf" 36))
(def font *font/liberation/serif/italic/36* ("font/LiberationSerif-Italic.ttf" 36))
(def font *font/liberation/serif/bold/36* ("font/LiberationSerif-Bold.ttf" 36))

(def font *font/liberation/serif/regular/42* ("font/LiberationSerif-Regular.ttf" 42))
(def font *font/liberation/serif/italic/42* ("font/LiberationSerif-Italic.ttf" 42))
(def font *font/liberation/serif/bold/42* ("font/LiberationSerif-Bold.ttf" 42))
