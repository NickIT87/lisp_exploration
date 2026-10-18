#|
Занятие 4. Сортировка пузырьком (Bubble Sort)
Условие. Отсортировать список чисел по возрастанию, не изменяя исходный список.
Идея. Сравниваем соседние элементы. Если левый больше правого, меняем их местами.
После каждого прохода самый большой из ещё не отсортированных элементов оказывается в конце.
Для удобства алгоритм работает с вектором — массивом с индексами.
В конце преобразуем результат обратно в список.

Что изучить:
- coerce — преобразование списка в вектор и обратно.
- aref — доступ к элементу вектора по индексу.
- rotatef — обмен значениями двух мест.
- below и downto — границы циклов.
Сложность: O(n^2) по времени, O(n) дополнительной памяти на копию вектора.
|#

(defun bubble-sort (items)
  (let* ((data (coerce items 'vector))
         (n (length data)))
    (loop for end from (1- n) downto 1 do
      (loop for i from 0 below end do
        (when (> (aref data i)
                 (aref data (1+ i)))
          (rotatef (aref data i)
                   (aref data (1+ i)))))
    )
    (coerce data 'list)))

;; Тесты
(assert (equal (bubble-sort '(5 1 4 2 8))
               '(1 2 4 5 8)))

(assert (equal (bubble-sort '(3 3 1 2 1))
               '(1 1 2 3 3)))

(assert (equal (bubble-sort '(-3 0 -10 5))
               '(-10 -3 0 5)))

(assert (equal (bubble-sort '()) '()))
(assert (equal (bubble-sort '(7)) '(7)))

;; Исходный список не изменился
(let ((items '(3 1 2)))
  (bubble-sort items)
  (assert (equal items '(3 1 2))))

(format t "All tests passed!~%")