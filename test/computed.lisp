(in-package :projectured.test)

(def projection json/number->graphics/text ()
  ())

(def projection error-handling ()
  ((projection :type projection)))

(def printer json/number->graphics/text ()
  (bind ((output (as (make-graphics/text 0 (as (print :CALCULATING-TEXT) (write-to-string (/ 1 (value-of -input-))))))))
    (make-iomap -input- nil nil nil output)))

(def printer error-handling ()
  (bind ((content-iomap (as (print :CALCULATING-CONTENT-IOMAP) (call-printer (projection-of -projection-) -recursion- -input- -input-reference-)))
         (output (as* (:handler #t)
                   (print :CALCULATING-OUTPUT)
                   (block nil
                     (handler-bind ((error (lambda (error)
                                             (return
                                               (make-graphics/text 0 (write-to-string error))))))
                       (output-of (va content-iomap)))))))
    (make-iomap/content -projection- -recursion- -input- -input-reference- output content-iomap)))

(def reader json/number->graphics/text ()
  (values))

(def reader error-handling ()
  (values))

(def test test/computed ()
  (bind ((projection (make-projection 'error-handling :projection (make-projection 'json/number->graphics/text)))
         (input (make-json/number (as 1)))
         (iomap (apply-printer input projection)))
    (is (string= (text-of (output-of iomap)) "1"))
    (setf (value-of input) 2)
    (is (string= (text-of (output-of iomap)) "1/2"))
    (setf (value-of input) 1)
    (is (string= (text-of (output-of iomap)) "1"))
    (setf (value-of input) 0)
    (ignore-errors (text-of (output-of iomap)))
    (is (search "DIVISION-BY-ZERO" (text-of (output-of iomap))))))
