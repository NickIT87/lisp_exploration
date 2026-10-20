
;;;; simple-dsl.lisp
;;;; Простой DSL с собственным текстовым синтаксисом и парсером.
;;;; Запуск: sbcl --script simple-dsl.lisp

(defpackage :simple-dsl
  (:use :cl))

(in-package :simple-dsl)

;;; 1. Исходный текст DSL

(defparameter *source*
  "# Example DSL

set points = 1500
set order_total = 120

rule vip_discount
  when points >= 1000
  then set discount = 20

rule free_shipping
  when order_total >= 50
  then set shipping = 0

rule premium_discount
  when points >= 2000
  then set discount = 30
")

;;; 2. Данные, которые создаёт парсер

(defstruct rule
  name
  variable
  operator
  threshold
  action-variable
  action-value)

(defparameter *env*
  (make-hash-table :test #'equal))

;;; 3. Разбиение строки на токены

(defun tokenize (line)
  "Превращает строку в список токенов.
   Символ # начинает комментарий."

  (let* ((comment-pos (position #\# line))
         (text (subseq line 0 comment-pos)))
    (with-input-from-string (stream text)
      (loop for token = (read stream nil :eof)
            until (eq token :eof)
            collect (string-upcase
                     (princ-to-string token))))))

;;; 4. Чтение строк DSL

(defun read-dsl-lines (source)
  (with-input-from-string (stream source)
    (loop for line = (read-line stream nil nil)
          while line
          for tokens = (tokenize line)
          when tokens
          collect tokens)))

;;; 5. Проверка синтаксиса

(defun expect (actual expected line-number)
  (unless (equal actual expected)
    (error "Syntax error at DSL line ~a: expected ~a, got ~a"
           line-number expected actual)))

(defun parse-set (tokens line-number)
  ;; Синтаксис: set variable = integer
  (unless (and (= (length tokens) 4)
               (equal (first tokens) "SET")
               (equal (third tokens) "="))
    (error "Invalid SET statement at line ~a: ~a"
           line-number tokens))

  (list :set
        (second tokens)
        (parse-integer (fourth tokens))))

;;; 6. Парсер всего DSL

(defun parse-dsl (source)
  (let ((lines (read-dsl-lines source))
        (rules nil)
        (assignments nil)
        (line-number 0))

    (loop while lines do
      (let ((tokens (pop lines)))
        (incf line-number)

        (cond
          ;; Начальное присваивание
          ((equal (first tokens) "SET")
           (push (parse-set tokens line-number)
                 assignments))

          ;; Объявление правила
          ((equal (first tokens) "RULE")
           (unless (= (length tokens) 2)
             (error "Expected: rule NAME at line ~a"
                    line-number))

           (let ((name (second tokens))
                 (condition nil)
                 (action nil))

             ;; Следующая строка: when variable OP number
             (unless lines
               (error "Missing WHEN for rule ~a" name))

             (let ((when-line (pop lines)))
               (incf line-number)

               (unless (and (= (length when-line) 4)
                            (equal (first when-line) "WHEN"))
                 (error "Expected WHEN at line ~a"
                        line-number))

               (let ((operator (third when-line)))
                 (unless (member operator
                                 '(">=" "<=" ">" "<" "=")
                                 :test #'equal)
                   (error "Unsupported operator: ~a" operator))

                 (setf condition
                       (list (second when-line)
                             operator
                             (parse-integer (fourth when-line))))))

             ;; Следующая строка: then set variable = number
             (unless lines
               (error "Missing THEN for rule ~a" name))

             (let ((then-line (pop lines)))
               (incf line-number)

               (unless (and (= (length then-line) 5)
                            (equal (first then-line) "THEN")
                            (equal (second then-line) "SET")
                            (equal (fourth then-line) "="))
                 (error "Expected THEN SET variable = number at line ~a"
                        line-number))

               (setf action
                     (list (third then-line)
                           (parse-integer (fifth then-line)))))

             (push
              (make-rule
               :name name
               :variable (first condition)
               :operator (second condition)
               :threshold (third condition)
               :action-variable (first action)
               :action-value (second action))
              rules)))

          (t
           (error "Unknown statement at line ~a: ~a"
                  line-number tokens)))))

    (values (nreverse assignments)
            (nreverse rules))))

;;; 7. Выполнение присваиваний

(defun set-variable (name value)
  (setf (gethash name *env*) value)
  (format t "  ~a = ~a~%" name value))

(defun get-variable (name)
  (multiple-value-bind (value found)
      (gethash name *env*)
    (unless found
      (error "Unknown variable: ~a" name))
    value))

;;; 8. Вычисление условия

(defun evaluate-condition (rule)
  (let ((value (get-variable (rule-variable rule)))
        (threshold (rule-threshold rule)))
    (cond
      ((equal (rule-operator rule) ">=")
       (>= value threshold))
      ((equal (rule-operator rule) "<=")
       (<= value threshold))
      ((equal (rule-operator rule) ">")
       (> value threshold))
      ((equal (rule-operator rule) "<")
       (< value threshold))
      ((equal (rule-operator rule) "=")
       (= value threshold))
      (t
       (error "Unknown comparison operator")))))

;;; 9. Исполнение правила

(defun execute-rule (rule)
  (format t "~%Checking rule: ~a~%"
          (rule-name rule))

  (if (evaluate-condition rule)
      (progn
        (format t "  MATCH~%")
        (set-variable
         (rule-action-variable rule)
         (rule-action-value rule)))
      (format t "  SKIP~%")))

;;; 10. Главная функция

(defun main ()
  (format t "=== Parsing DSL ===~%")

  (multiple-value-bind (assignments rules)
      (parse-dsl *source*)

    (format t "Parsed assignments: ~a~%"
            (length assignments))
    (format t "Parsed rules: ~a~%"
            (length rules))

    (format t "~%=== Initial state ===~%")
    (dolist (assignment assignments)
      (set-variable (second assignment)
                    (third assignment)))

    (format t "~%=== Execute rules ===~%")
    (dolist (rule rules)
      (execute-rule rule))

    (format t "~%=== Final state ===~%")
    (maphash
     (lambda (name value)
       (format t "~a = ~a~%" name value))
     *env*)))

(main)

#|
[Running] sbcl --script simple_dsl.lisp
=== Parsing DSL ===
Parsed assignments: 2
Parsed rules: 3

=== Initial state ===
  POINTS = 1500
  ORDER_TOTAL = 120

=== Execute rules ===

Checking rule: VIP_DISCOUNT
  MATCH
  DISCOUNT = 20

Checking rule: FREE_SHIPPING
  MATCH
  SHIPPING = 0

Checking rule: PREMIUM_DISCOUNT
  SKIP

=== Final state ===
POINTS = 1500
ORDER_TOTAL = 120
DISCOUNT = 20
SHIPPING = 0

[Done] exited with code=0 in 0.09 seconds
|#