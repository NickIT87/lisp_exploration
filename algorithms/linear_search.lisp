#|
Модуль 1. Поиск и обработка списков
Занятие 1. Линейный поиск (Linear Search)
Условие. Дан список чисел и искомое число. 
Нужно вернуть индекс первого найденного элемента. 
Если элемента нет, вернуть nil. Индексация начинается с нуля.
Например, для списка (10 20 30 40) поиск числа 30 должен вернуть 2.
Идея. Последовательно перебираем элементы, сравниваем каждый с искомым 
числом и останавливаемся при первом совпадении.

Что изучить:
- defun — определение функции.
- loop — цикл.
- for index from 0 — счётчик индексов.
- return — выход из цикла с результатом.
- nil — отсутствие найденного значения.
Сложность: \(O(n)\) по времени, где \(n\) — количество элементов.
|#

(defun linear-search (items target)
  (loop for item in items
        for index from 0
        when (eql item target)
          do (return index)
        finally (return nil)))

;; Тесты
(assert (eql (linear-search '(10 20 30 40) 30) 2))
(assert (eql (linear-search '(10 20 30 40) 10) 0))
(assert (null (linear-search '(10 20 30 40) 99)))
(assert (null (linear-search '() 10)))

(format t "All tests passed!~%")
(print (linear-search '(10 20 30 40) 30))

; [Running] sbcl --script linear_search.lisp
; All tests passed!
; 2 
; [Done] exited with code=0 in 0.067 seconds