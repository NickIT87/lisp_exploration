(defun factorial (n)
  "Calculate the factorial of a non-negative integer N."
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
      
;; Example usage:
(format t "Factorial of 5: ~a~%" (factorial 5))
