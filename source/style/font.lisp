;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document style/font ()
  ((filename :type string)
   (size :type number)
   (raw :type t)))

;;;;;;
;;; Construction

(def (function e) make-style/font (filename size)
  (make-instance 'style/font
                 :filename filename
                 :size size
                 :raw nil))

;;;;;;
;;; API

(def (generic e) measure-text (text font)
  (:documentation "Measures the size of TEXT drawn with FONT."))

;;;;;;
;;; Definer

(def (definer e :available-flags "e") font (name (filename size))
  `(def special-variable ,name (make-style/font (system-relative-pathname :projectured ,filename) ,size)))

;;;;;;
;;; Default

(def font *font/default* ("etc/UbuntuMono-R.ttf" 18))

;;;;;;
;;; Inconsolata

(def font *font/inconsolata/regular/18* ("etc/Inconsolata.otf" 18))

;;;;;;
;;; Ubuntu

(def font *font/ubuntu/monospace/18* ("etc/UbuntuMono-R.ttf" 18))
(def font *font/ubuntu/regular/18* ("etc/Ubuntu-R.ttf" 18))
(def font *font/ubuntu/bold/36* ("etc/Ubuntu-B.ttf" 36))
(def font *font/ubuntu/bold/24* ("etc/Ubuntu-R.ttf" 24))
