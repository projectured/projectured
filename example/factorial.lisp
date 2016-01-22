(defun factorial (n)
  "The FACTORIAL function computes the product of the integers between 1 and N."
  (if (< n 2)
      1
      (* n (factorial (- n 1)))))
