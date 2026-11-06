#|
Занятие 7. Разворот списка и палиндром
Условие. Написать функцию, которая разворачивает список,
и функцию, которая проверяет, читается ли список одинаково
слева направо и справа налево.

Например, (1 2 3 2 1) — палиндром, а (1 2 3) — нет.

Идея. Для разворота рекурсивно обрабатываем остаток списка
и добавляем первый элемент в конец результата.
Для проверки палиндрома сравниваем список с его развёрнутой копией.

Что изучить:
- append — объединение списков.
- equal — структурное сравнение списков.
- not — логическое отрицание.
- Суффикс -p в названии функции — распространённое соглашение для предикатов,
то есть функций, проверяющих условие.
|#

; У этой реализации сложность O(n²), поскольку append каждый раз копирует список.

; (defun reverse-list (items)
;   (if (null items)
;       '()
;       (append (reverse-list (rest items))
;               (list (first items)))))

; более эффективная реализация со сложностью O(n), можно использовать аккумулятор:
(defun reverse-list (items)
  (labels ((helper (items result)
             (if (null items)
                 result
                 (helper (rest items)
                         (cons (first items) result)))))
    (helper items '())))

(defun palindrome-p (items)
  (equal items (reverse-list items)))

;; Тесты разворота
(assert (equal (reverse-list '(1 2 3))
               '(3 2 1)))

(assert (equal (reverse-list '(a b c d))
               '(d c b a)))

(assert (equal (reverse-list '()) '()))

;; Тесты палиндрома
(assert (palindrome-p '(1 2 3 2 1)))
(assert (palindrome-p '(a b a)))
(assert (palindrome-p '()))
(assert (not (palindrome-p '(1 2 3))))
(assert (not (palindrome-p '(a b c))))

(format t "All tests passed!~%")