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

(def (font e) *font/default* ("etc/UbuntuMono-R.ttf" 18))

;;;;;;
;;; Inconsolata

(def (font e) *font/inconsolata/regular/18* ("etc/Inconsolata.otf" 18))

;;;;;;
;;; Ubuntu monospace

(def (font e) *font/ubuntu/monospace/regular/14* ("etc/UbuntuMono-R.ttf" 14))
(def (font e) *font/ubuntu/monospace/italic/14* ("etc/UbuntuMono-RI.ttf" 14))
(def (font e) *font/ubuntu/monospace/bold/14* ("etc/UbuntuMono-B.ttf" 14))

(def (font e) *font/ubuntu/monospace/regular/18* ("etc/UbuntuMono-R.ttf" 18))
(def (font e) *font/ubuntu/monospace/italic/18* ("etc/UbuntuMono-RI.ttf" 18))
(def (font e) *font/ubuntu/monospace/bold/18* ("etc/UbuntuMono-B.ttf" 18))

;;;;;;
;;; Ubuntu

(def (font e) *font/ubuntu/regular/14* ("etc/Ubuntu-R.ttf" 14))
(def (font e) *font/ubuntu/italic/14* ("etc/Ubuntu-RI.ttf" 14))
(def (font e) *font/ubuntu/bold/14* ("etc/Ubuntu-B.ttf" 14))

(def (font e) *font/ubuntu/regular/18* ("etc/Ubuntu-R.ttf" 18))
(def (font e) *font/ubuntu/italic/18* ("etc/Ubuntu-RI.ttf" 18))
(def (font e) *font/ubuntu/bold/18* ("etc/Ubuntu-B.ttf" 18))

(def (font e) *font/ubuntu/regular/24* ("etc/Ubuntu-R.ttf" 24))
(def (font e) *font/ubuntu/italic/24* ("etc/Ubuntu-RI.ttf" 24))
(def (font e) *font/ubuntu/bold/24* ("etc/Ubuntu-B.ttf" 24))

(def (font e) *font/ubuntu/regular/36* ("etc/Ubuntu-R.ttf" 36))
(def (font e) *font/ubuntu/italic/36* ("etc/Ubuntu-RI.ttf" 36))
(def (font e) *font/ubuntu/bold/36* ("etc/Ubuntu-B.ttf" 36))

;;;;;;
;;; Liberation serif

(def (font e) *font/liberation/serif/regular/14* ("etc/LiberationSerif-Regular.ttf" 14))
(def (font e) *font/liberation/serif/italic/14* ("etc/LiberationSerif-Italic.ttf" 14))
(def (font e) *font/liberation/serif/bold/14* ("etc/LiberationSerif-Bold.ttf" 14))

(def (font e) *font/liberation/serif/regular/18* ("etc/LiberationSerif-Regular.ttf" 18))
(def (font e) *font/liberation/serif/italic/18* ("etc/LiberationSerif-Italic.ttf" 18))
(def (font e) *font/liberation/serif/bold/18* ("etc/LiberationSerif-Bold.ttf" 18))

(def (font e) *font/liberation/serif/regular/24* ("etc/LiberationSerif-Regular.ttf" 24))
(def (font e) *font/liberation/serif/italic/24* ("etc/LiberationSerif-Italic.ttf" 24))
(def (font e) *font/liberation/serif/bold/24* ("etc/LiberationSerif-Bold.ttf" 24))

(def (font e) *font/liberation/serif/regular/30* ("etc/LiberationSerif-Regular.ttf" 30))
(def (font e) *font/liberation/serif/italic/30* ("etc/LiberationSerif-Italic.ttf" 30))
(def (font e) *font/liberation/serif/bold/30* ("etc/LiberationSerif-Bold.ttf" 30))

(def (font e) *font/liberation/serif/regular/36* ("etc/LiberationSerif-Regular.ttf" 36))
(def (font e) *font/liberation/serif/italic/36* ("etc/LiberationSerif-Italic.ttf" 36))
(def (font e) *font/liberation/serif/bold/36* ("etc/LiberationSerif-Bold.ttf" 36))
