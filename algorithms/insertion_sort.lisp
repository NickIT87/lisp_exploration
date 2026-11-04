#|
Занятие 5. Сортировка вставками (Insertion Sort)

Условие. Реализовать сортировку по возрастанию другим способом — сортировкой вставками.

Идея. Поддерживаем отсортированную часть списка.
Каждый следующий элемент вставляем на подходящее место в уже отсортированной части.

В этом варианте использовать рекурсию и стандартную функцию reduce не требуется:
достаточно обычных списков и сравнений.

Что изучить:
- cons — создание новой пары или добавление элемента в начало списка.
- first и rest — первый элемент и оставшаяся часть списка.
- cond — несколько ветвей условий.
- Рекурсивное построение нового списка.
Сложность: O(n^2) по времени в худшем случае. Здесь мы создаём новые списки при вставке.
|#

(defun insert-sorted (item sorted)
  (cond
    ((null sorted)
     (list item))
    ((<= item (first sorted))
     (cons item sorted))
    (t
     (cons (first sorted)
           (insert-sorted item (rest sorted))))))

(defun insertion-sort (items)
  (let ((result '()))
    (dolist (item items)
      (setf result (insert-sorted item result)))
    result))

;; Тесты
(assert (equal (insertion-sort '(5 2 4 6 1 3))
               '(1 2 3 4 5 6)))

(assert (equal (insertion-sort '(3 1 3 2 1))
               '(1 1 2 3 3)))

(assert (equal (insertion-sort '(-4 0 -2 8))
               '(-4 -2 0 8)))

(assert (equal (insertion-sort '()) '()))
(assert (equal (insertion-sort '(9)) '(9)))

(format t "All tests passed!~%")