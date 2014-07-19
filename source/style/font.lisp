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
  `(def special-variable ,name (make-style/font ,filename ,size)))

;;;;;;
;;; Default

(def (font e) *font/default* ("font/UbuntuMono-R.ttf" 18))

;;;;;;
;;; Inconsolata

(def (font e) *font/inconsolata/regular/18* ("font/Inconsolata.otf" 18))

;;;;;;
;;; Ubuntu monospace

(def (font e) *font/ubuntu/monospace/regular/14* ("font/UbuntuMono-R.ttf" 14))
(def (font e) *font/ubuntu/monospace/italic/14* ("font/UbuntuMono-RI.ttf" 14))
(def (font e) *font/ubuntu/monospace/bold/14* ("font/UbuntuMono-B.ttf" 14))

(def (font e) *font/ubuntu/monospace/regular/18* ("font/UbuntuMono-R.ttf" 18))
(def (font e) *font/ubuntu/monospace/italic/18* ("font/UbuntuMono-RI.ttf" 18))
(def (font e) *font/ubuntu/monospace/bold/18* ("font/UbuntuMono-B.ttf" 18))

;;;;;;
;;; Ubuntu

(def (font e) *font/ubuntu/regular/14* ("font/Ubuntu-R.ttf" 14))
(def (font e) *font/ubuntu/italic/14* ("font/Ubuntu-RI.ttf" 14))
(def (font e) *font/ubuntu/bold/14* ("font/Ubuntu-B.ttf" 14))

(def (font e) *font/ubuntu/regular/18* ("font/Ubuntu-R.ttf" 18))
(def (font e) *font/ubuntu/italic/18* ("font/Ubuntu-RI.ttf" 18))
(def (font e) *font/ubuntu/bold/18* ("font/Ubuntu-B.ttf" 18))

(def (font e) *font/ubuntu/regular/24* ("font/Ubuntu-R.ttf" 24))
(def (font e) *font/ubuntu/italic/24* ("font/Ubuntu-RI.ttf" 24))
(def (font e) *font/ubuntu/bold/24* ("font/Ubuntu-B.ttf" 24))

(def (font e) *font/ubuntu/regular/36* ("font/Ubuntu-R.ttf" 36))
(def (font e) *font/ubuntu/italic/36* ("font/Ubuntu-RI.ttf" 36))
(def (font e) *font/ubuntu/bold/36* ("font/Ubuntu-B.ttf" 36))

;;;;;;
;;; Liberation serif

(def (font e) *font/liberation/serif/regular/14* ("font/LiberationSerif-Regular.ttf" 14))
(def (font e) *font/liberation/serif/italic/14* ("font/LiberationSerif-Italic.ttf" 14))
(def (font e) *font/liberation/serif/bold/14* ("font/LiberationSerif-Bold.ttf" 14))

(def (font e) *font/liberation/serif/regular/18* ("font/LiberationSerif-Regular.ttf" 18))
(def (font e) *font/liberation/serif/italic/18* ("font/LiberationSerif-Italic.ttf" 18))
(def (font e) *font/liberation/serif/bold/18* ("font/LiberationSerif-Bold.ttf" 18))

(def (font e) *font/liberation/serif/regular/24* ("font/LiberationSerif-Regular.ttf" 24))
(def (font e) *font/liberation/serif/italic/24* ("font/LiberationSerif-Italic.ttf" 24))
(def (font e) *font/liberation/serif/bold/24* ("font/LiberationSerif-Bold.ttf" 24))

(def (font e) *font/liberation/serif/regular/30* ("font/LiberationSerif-Regular.ttf" 30))
(def (font e) *font/liberation/serif/italic/30* ("font/LiberationSerif-Italic.ttf" 30))
(def (font e) *font/liberation/serif/bold/30* ("font/LiberationSerif-Bold.ttf" 30))

(def (font e) *font/liberation/serif/regular/36* ("font/LiberationSerif-Regular.ttf" 36))
(def (font e) *font/liberation/serif/italic/36* ("font/LiberationSerif-Italic.ttf" 36))
(def (font e) *font/liberation/serif/bold/36* ("font/LiberationSerif-Bold.ttf" 36))
