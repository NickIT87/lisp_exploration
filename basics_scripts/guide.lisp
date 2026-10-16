;;;; ==========================================
;;;; Common Lisp — базовый пример в одном файле
;;;; ==========================================


;;;; 1. ПЕРЕМЕННЫЕ

(defparameter *name* "Alex")
(defparameter *age* 25)

(format t "~%=== Variables ===~%")
(format t "Name: ~a, Age: ~a~%" *name* *age*)


;;;; 2. ЛОКАЛЬНЫЕ ПЕРЕМЕННЫЕ: LET

(let ((x 10)
      (y 20))
  (format t "Sum: ~a~%" (+ x y)))


;;;; 3. АРИФМЕТИКА

(format t "~%=== Arithmetic ===~%")
(format t "Addition: ~a~%" (+ 10 5))
(format t "Subtraction: ~a~%" (- 10 5))
(format t "Multiplication: ~a~%" (* 10 5))
(format t "Division: ~a~%" (/ 10 5))


;;;; 4. УСЛОВИЯ: IF, COND, WHEN, UNLESS

(format t "~%=== Conditions ===~%")

(if (>= *age* 18)
    (format t "Adult~%")
    (format t "Minor~%"))

(cond
  ((< *age* 18) (format t "Young~%"))
  ((< *age* 30) (format t "Adult~%"))
  (t             (format t "Senior~%")))

(when (> *age* 20)
  (format t "Age is greater than 20~%"))

(unless (< *age* 18)
  (format t "Access granted~%"))


;;;; 5. ФУНКЦИИ: DEFUN

(defun greet (name)
  (format nil "Hello, ~a!" name))

(defun add (a b)
  (+ a b))

(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

(format t "~%=== Functions ===~%")
(format t "~a~%" (greet "Alex"))
(format t "Sum: ~a~%" (add 10 20))
(format t "Factorial: ~a~%" (factorial 5))


;;;; 6. НЕОБЯЗАТЕЛЬНЫЕ АРГУМЕНТЫ

(defun greet-with (name &optional (greeting "Hello"))
  (format nil "~a, ~a!" greeting name))

(format t "~a~%" (greet-with "Alex"))
(format t "~a~%" (greet-with "Alex" "Welcome"))


;;;; 7. УПРАВЛЕНИЕ ПОТОКОМ: PROGN

(progn
  (format t "~%=== PROGN ===~%")
  (format t "First expression~%")
  (format t "Second expression~%"))


;;;; 8. ЦИКЛ LOOP

(format t "~%=== LOOP ===~%")

(loop for i from 1 to 5
      do (format t "~a " i))

(terpri)


;;;; 9. DOTIMES

(dotimes (i 3)
  (format t "Iteration: ~a~%" i))


;;;; 10. DOLIST

(format t "~%=== DOLIST ===~%")

(dolist (fruit '("Apple" "Banana" "Orange"))
  (format t "~a~%" fruit))


;;;; 11. СПИСКИ

(defparameter *numbers* '(1 2 3 4 5))

(format t "~%=== Lists ===~%")
(format t "List: ~a~%" *numbers*)
(format t "First: ~a~%" (car *numbers*))
(format t "Rest: ~a~%" (cdr *numbers*))
(format t "Length: ~a~%" (length *numbers*))
(format t "Reverse: ~a~%" (reverse *numbers*))
(format t "Add element: ~a~%" (cons 0 *numbers*))

(format t "Squares: ~a~%"
        (mapcar (lambda (x) (* x x))
                *numbers*))

(format t "Sum: ~a~%"
        (reduce #'+ *numbers*))


;;;; 12. ИЗМЕНЕНИЕ ДАННЫХ: SETF, INCF, PUSH

(format t "~%=== Mutable data ===~%")

(let ((counter 0)
      (items '(2 3 4)))

  (incf counter)
  (incf counter 5)

  (push 1 items)

  (format t "Counter: ~a~%" counter)
  (format t "Items: ~a~%" items))


;;;; 13. МАССИВЫ

(format t "~%=== Arrays ===~%")

(let ((numbers (vector 10 20 30)))

  (format t "First: ~a~%" (aref numbers 0))

  (setf (aref numbers 1) 99)

  (format t "Updated array: ~a~%" numbers))


;;;; 14. ХЕШ-ТАБЛИЦА

(format t "~%=== Hash table ===~%")

(let ((user (make-hash-table :test 'equal)))

  (setf (gethash "name" user) "Alex")
  (setf (gethash "role" user) "admin")

  (format t "Name: ~a~%" (gethash "name" user))
  (format t "Role: ~a~%" (gethash "role" user))

  (maphash
    (lambda (key value)
      (format t "~a = ~a~%" key value))
    user))


;;;; 15. СТРУКТУРЫ: DEFSTRUCT

(defstruct person
  name
  age)

(format t "~%=== Structures ===~%")

(let ((p (make-person :name "Bob" :age 30)))

  (format t "Name: ~a~%" (person-name p))
  (format t "Age: ~a~%" (person-age p))

  (setf (person-age p) 31)

  (format t "Updated age: ~a~%" (person-age p)))


;;;; 16. ЛЯМБДА-ФУНКЦИИ

(format t "~%=== Lambda ===~%")

(format t "Result: ~a~%"
        (funcall (lambda (x) (* x 2)) 10))

(format t "Apply: ~a~%"
        (apply #'+ '(1 2 3 4)))


;;;; 17. МАКРОСЫ: DEFMACRO

(defmacro twice (&body body)
  `(progn
     ,@body
     ,@body))

(format t "~%=== Macro ===~%")

(twice
  (format t "Executed twice!~%"))


;;;; 18. ОБРАБОТКА ОШИБОК

(format t "~%=== Error handling ===~%")

(handler-case
    (/ 10 0)

  (division-by-zero (condition)
    (format t "Caught error: ~a~%" condition)))


;;;; 19. РАБОТА СО СТРОКАМИ

(format t "~%=== Strings ===~%")

(format t "Uppercase: ~a~%"
        (string-upcase "hello lisp"))

(format t "Combined: ~a~%"
        (concatenate 'string "Hello, " "World!"))

(format t "Formatted: ~a~%"
        (format nil "Name: ~a, Age: ~a" *name* *age*))


;;;; 20. РАБОТА С ФАЙЛАМИ

(format t "~%=== File I/O ===~%")

;; Создаём файл и записываем текст.

(with-open-file (stream "report.txt"
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)

  (format stream "Report~%")
  (format stream "Name: ~a~%" *name*)
  (format stream "Age: ~a~%" *age*))

(format t "File report.txt created.~%")


;;;; 21. QUOTE И EVAL

(format t "~%=== Quote and Eval ===~%")

(format t "Quoted: ~a~%" '(+ 2 3))
(format t "Evaluated: ~a~%" (eval '(+ 2 3)))


;;;; 22. ЗАВЕРШЕНИЕ

(format t "~%=== Program finished ===~%")

; === Variables ===
; Name: Alex, Age: 25
; Sum: 30

; === Arithmetic ===
; Addition: 15
; Subtraction: 5
; Multiplication: 50
; Division: 2

; === Conditions ===
; Adult
; Adult
; Age is greater than 20
; Access granted

; === Functions ===
; Hello, Alex!
; Sum: 30
; Factorial: 120
; Hello, Alex!
; Welcome, Alex!

; === PROGN ===
; First expression
; Second expression

; === LOOP ===
; 1 2 3 4 5 
; Iteration: 0
; Iteration: 1
; Iteration: 2

; === DOLIST ===
; Apple
; Banana
; Orange

; === Lists ===
; List: (1 2 3 4 5)
; First: 1
; Rest: (2 3 4 5)
; Length: 5
; Reverse: (5 4 3 2 1)
; Add element: (0 1 2 3 4 5)
; Squares: (1 4 9 16 25)
; Sum: 15

; === Mutable data ===
; Counter: 6
; Items: (1 2 3 4)

; === Arrays ===
; First: 10
; Updated array: #(10 99 30)

; === Hash table ===
; Name: Alex
; Role: admin
; name = Alex
; role = admin

; === Structures ===
; Name: Bob
; Age: 30
; Updated age: 31

; === Lambda ===
; Result: 20
; Apply: 10

; === Macro ===
; Executed twice!
; Executed twice!

; === Error handling ===

; ; file: /Users/nick/programming/lisp_exploration/basics_scripts/guide.lisp
; ; in: HANDLER-CASE (/ 10 0)
; ;     (/ 10 0)
; ; 
; ; caught WARNING:
; ;   division by zero
; ; 
; ; compilation unit finished
; ;   caught 1 WARNING condition
; Caught error: arithmetic error DIVISION-BY-ZERO signalled
; Operation was (/ 10 0).

; === Strings ===
; Uppercase: HELLO LISP
; Combined: Hello, World!
; Formatted: Name: Alex, Age: 25

; === File I/O ===
; File report.txt created.

; === Quote and Eval ===
; Quoted: (+ 2 3)
; Evaluated: 5

; === Program finished ===
