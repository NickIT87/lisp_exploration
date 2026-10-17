#|
Занятие 2. Поиск максимума и минимума
Условие. Найти максимальное и минимальное числа в списке.
Для пустого списка вернуть nil в обоих случаях.
Идея. Запоминаем первый элемент в качестве начального максимума и минимума.
Затем сравниваем с ним остальные элементы.

Что изучить:
- let — создание локальных переменных.
- dolist — перебор списка.
- setf — изменение значения переменной.
- values — возврат нескольких значений из функции.
- multiple-value-bind — получение нескольких результатов.
Сложность: O(n) по времени и O(1) дополнительной памяти.
|#

(defun find-min-max (items)
  (if (null items)
      (values nil nil)
      (let ((minimum (first items))
            (maximum (first items)))
        (dolist (item (rest items))
          (when (< item minimum)
            (setf minimum item))
          (when (> item maximum)
            (setf maximum item)))
        (values minimum maximum))))

;; Тесты
(multiple-value-bind (minimum maximum)
    (find-min-max '(7 2 9 4 1 8))
  (assert (= minimum 1))
  (assert (= maximum 9)))

(multiple-value-bind (minimum maximum)
    (find-min-max '(-5 -2 -10))
  (assert (= minimum -10))
  (assert (= maximum -2)))

(multiple-value-bind (minimum maximum)
    (find-min-max '())
  (assert (null minimum))
  (assert (null maximum)))

(format t "All tests passed!~%")
(print (multiple-value-list
        (find-min-max '(-5 -2 -10 2))))

; [Running] sbcl --script find_min_max.lisp
; All tests passed!
; (-10 2) 
; [Done] exited with code=0 in 0.1 seconds
