
;;;; dsl.lisp
;;;; Минимальный DSL для бизнес-правил на Common Lisp
;;;; Запуск: sbcl --script dsl.lisp

(defpackage :business-dsl
  (:use :cl))

(in-package :business-dsl)

;;; 1. Окружение: переменные DSL и их значения

(defparameter *env*
  (make-hash-table :test #'equal))

(defun env-get (name)
  (multiple-value-bind (value found-p)
      (gethash name *env*)
    (if found-p
        value
        (error "Unknown variable: ~a" name))))

(defun env-set (name value)
  (setf (gethash name *env*) value))

;;; 2. Реестр правил

(defparameter *rules* nil)

(defun register-rule (name condition action)
  ;; Повторное объявление заменяет старое правило.
  (setf *rules*
        (acons name
               (list condition action)
               (remove name *rules* :key #'car :test #'equal)))
  name)

;;; 3. Интерпретатор выражений DSL

(defun eval-expr (expr)
  (cond
    ;; Литералы
    ((numberp expr) expr)
    ((stringp expr) expr)
    ((eq expr t) t)
    ((null expr) nil)

    ;; Переменные DSL
    ((symbolp expr)
     (env-get (symbol-name expr)))

    ;; Составные выражения
    ((consp expr)
     (let ((op (car expr))
           (args (cdr expr)))
       (case op
         (and
          (every #'eval-expr args))

         (or
          (some #'eval-expr args))

         (not
          (not (eval-expr (first args))))

         (>
          (> (eval-expr (first args))
             (eval-expr (second args))))

         (>=
          (>= (eval-expr (first args))
              (eval-expr (second args))))

         (<
          (< (eval-expr (first args))
             (eval-expr (second args))))

         (<=
          (<= (eval-expr (first args))
              (eval-expr (second args))))

         (=
          (= (eval-expr (first args))
             (eval-expr (second args))))

         (+
          (apply #'+ (mapcar #'eval-expr args)))

         (-
          (apply #'- (mapcar #'eval-expr args)))

         (*
          (apply #'* (mapcar #'eval-expr args)))

         (otherwise
          (error "Unknown DSL operator: ~a" op)))))

    (t
     (error "Invalid DSL expression: ~a" expr))))

;;; 4. Исполнение действий DSL

(defun execute-action (action)
  (unless (and (consp action)
               (eq (car action) 'set)
               (= (length action) 3)
               (symbolp (second action)))
    (error "Invalid DSL action: ~a" action))

  (let ((name (symbol-name (second action)))
        (value (eval-expr (third action))))
    (env-set name value)
    (format t "  SET ~a = ~a~%" name value)))

;;; 5. Макрос — синтаксис DSL

(defmacro rule (name condition action)
  `(register-rule ',name ',condition ',action))

;;; 6. Выполнение всех правил

(defun run-rules ()
  (dolist (entry (reverse *rules*))
    (let* ((name (car entry))
           (condition (first (cdr entry)))
           (action (second (cdr entry))))
      (format t "Checking rule: ~a~%" name)
      (if (eval-expr condition)
          (progn
            (format t "  MATCH~%")
            (execute-action action))
          (format t "  SKIP~%")))))

;;; 7. Удобная печать значений

(defun show-variable (name)
  (format t "~a = ~a~%"
          name
          (env-get name)))

;;; 8. Начальные данные

(env-set "POINTS" 1500)
(env-set "ORDER-TOTAL" 120)
(env-set "DISCOUNT" 0)
(env-set "SHIPPING-COST" 10)

;;; 9. Объявления на языке DSL

(rule vip-discount
  (and (>= points 1000)
       (>= order-total 100))
  (set discount 20))

(rule free-shipping
  (>= order-total 50)
  (set shipping-cost 0))

(rule premium-customer
  (and (>= points 2000)
       (>= order-total 100))
  (set discount 30))

;;; 10. Запуск

(defun main ()
  (format t "~%=== BUSINESS DSL ===~%")
  (format t "~%--- Initial state ---~%")
  (show-variable "POINTS")
  (show-variable "ORDER-TOTAL")
  (show-variable "DISCOUNT")
  (show-variable "SHIPPING-COST")

  (format t "~%--- Executing rules ---~%")
  (run-rules)

  (format t "~%--- Final state ---~%")
  (show-variable "DISCOUNT")
  (show-variable "SHIPPING-COST")

  (format t "~%=== DONE ===~%"))

(main)

#|
[Running] sbcl --script dsl_example.lisp

=== BUSINESS DSL ===

--- Initial state ---
POINTS = 1500
ORDER-TOTAL = 120
DISCOUNT = 0
SHIPPING-COST = 10

--- Executing rules ---
Checking rule: VIP-DISCOUNT
  MATCH
  SET DISCOUNT = 20
Checking rule: FREE-SHIPPING
  MATCH
  SET SHIPPING-COST = 0
Checking rule: PREMIUM-CUSTOMER
  SKIP

--- Final state ---
DISCOUNT = 20
SHIPPING-COST = 0

=== DONE ===

[Done] exited with code=0 in 0.096 seconds
|#
