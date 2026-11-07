(defun binary-search (items target)
  (let* ((data (coerce items 'vector))
         (left 0)
         (right (1- (length data))))
    (loop while (<= left right) do
      (let ((middle (floor (+ left right) 2)))
        (cond
          ((= (aref data middle) target)
           (return-from binary-search middle))
          ((< (aref data middle) target)
           (setf left (1+ middle)))
          (t
           (setf right (1- middle))))))
    nil))

;; Тесты
(assert (= (binary-search '(1 3 5 7 9) 5) 2))
(assert (= (binary-search '(1 3 5 7 9) 1) 0))
(assert (= (binary-search '(1 3 5 7 9) 9) 4))
(assert (null (binary-search '(1 3 5 7 9) 4)))
(assert (null (binary-search '() 10)))

(format t "All tests passed!~%")