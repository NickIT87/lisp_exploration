#|
Занятие 3. Сумма и количество элементов
Условие. Для списка чисел вычислить сумму, количество элементов и среднее арифметическое. 
Для пустого списка среднее значение должно быть nil.
Идея. За один проход по списку накапливаем сумму и количество элементов.

Что изучить:
- incf — увеличить значение переменной.
- zerop — проверить, равен ли нулю аргумент.
- / — в Common Lisp деление целых чисел может возвращать точную дробь, например 3/2, а не 1.5.
Сложность: O(n) по времени и O(1) дополнительной памяти.
|#

(defun list-stats (items)
  (let ((sum 0)
        (count 0))
    (dolist (item items)
      (incf sum item)
      (incf count))
    (values sum
            count
            (if (zerop count)
                nil
                (/ sum count)))))

;; Тесты
(multiple-value-bind (sum count average)
    (list-stats '(2 4 6 8))
  (assert (= sum 20))
  (assert (= count 4))
  (assert (= average 5)))

(multiple-value-bind (sum count average)
    (list-stats '(1 2))
  (assert (= sum 3))
  (assert (= count 2))
  (assert (= average 3/2)))

(multiple-value-bind (sum count average)
    (list-stats '())
  (assert (= sum 0))
  (assert (= count 0))
  (assert (null average)))

(format t "All tests passed!~%")
(print (multiple-value-list
        (list-stats '(2 4 6 8))))

; [Running] sbcl --script list_stats.lisp
; All tests passed!
; (20 4 5) 
; [Done] exited with code=0 in 0.071 seconds
