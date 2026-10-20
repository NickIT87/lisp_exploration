
#lang racket

(require racket/string)

;; ==========================================
;; 1. Исходный текст DSL
;; ==========================================

(define source
  "
# Initial data
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

;; ==========================================
;; 2. Структуры данных
;; ==========================================

(struct assignment (name value) #:transparent)

(struct rule
  (name variable operator threshold action-variable action-value)
  #:transparent)

;; Окружение: имя переменной -> значение
(define env (make-hash))

;; ==========================================
;; 3. Лексический анализ
;; ==========================================

(define (tokenize line)
  ;; Удаляем комментарии, начинающиеся с #
  (define without-comment
    (regexp-replace #rx"#.*$" line ""))

  ;; Разбиваем строку по пробелам
  (define trimmed (string-trim without-comment))

  (if (string=? trimmed "")
      '()
      (string-split (string-upcase trimmed))))

;; ==========================================
;; 4. Вспомогательные функции парсера
;; ==========================================

(define (parse-integer token)
  (define value (string->number token))
  (unless (exact-integer? value)
    (error 'parser "Expected integer, got: ~a" token))
  value)

(define (expect tokens expected line-number)
  (unless (and (pair? tokens)
               (string=? (first tokens) expected))
    (error 'parser
           "Line ~a: expected ~a, got ~a"
           line-number expected tokens)))

(define (read-lines text)
  ;; Сохраняем номер каждой непустой строки
  (for/list ([line (in-list (string-split text "\n" #:trim? #f))]
             [line-number (in-naturals 1)]
             #:do [(define tokens (tokenize line))]
             #:when (pair? tokens))
    (list line-number tokens)))

;; ==========================================
;; 5. Парсер присваивания
;; ==========================================

;; Синтаксис:
;; set variable = integer

(define (parse-set tokens line-number)
  (unless (and (= (length tokens) 4)
               (string=? (first tokens) "SET")
               (string=? (third tokens) "="))
    (error 'parser
           "Line ~a: expected SET variable = integer"
           line-number))

  (assignment
   (string-downcase (second tokens))
   (parse-integer (fourth tokens))))

;; ==========================================
;; 6. Парсер DSL
;; ==========================================

(define (parse-dsl text)
  (define lines (read-lines text))

  (let loop ([remaining lines]
             [assignments '()]
             [rules '()])

    (cond
      [(null? remaining)
       (values (reverse assignments)
               (reverse rules))]

      [else
       (define current (first remaining))
       (define line-number (first current))
       (define tokens (second current))

       (cond
         ;; Начальное присваивание
         [(string=? (first tokens) "SET")
          (loop (rest remaining)
                (cons (parse-set tokens line-number)
                      assignments)
                rules)]

         ;; Объявление правила
         [(string=? (first tokens) "RULE")
          (unless (= (length tokens) 2)
            (error 'parser
                   "Line ~a: expected RULE name"
                   line-number))

          (define name
            (string-downcase (second tokens)))

          ;; Ожидаем WHEN
          (when (< (length remaining) 3)
            (error 'parser
                   "Incomplete rule: ~a"
                   name))

          (define when-entry (second remaining))
          (define when-line (first when-entry))
          (define when-tokens (second when-entry))

          (unless (= (length when-tokens) 4)
            (error 'parser
                   "Line ~a: expected WHEN variable operator integer"
                   when-line))

          (expect when-tokens "WHEN" when-line)

          (define operator (third when-tokens))

          (unless (member operator '(">=" "<=" ">" "<" "="))
            (error 'parser
                   "Line ~a: unsupported operator ~a"
                   when-line operator))

          (define variable
            (string-downcase (second when-tokens)))

          (define threshold
            (parse-integer (fourth when-tokens)))

          ;; Ожидаем THEN SET variable = integer
          (define then-entry (third remaining))
          (define then-line (first then-entry))
          (define then-tokens (second then-entry))

          (unless (and (= (length then-tokens) 5)
                       (string=? (first then-tokens) "THEN")
                       (string=? (second then-tokens) "SET")
                       (string=? (fourth then-tokens) "="))
            (error 'parser
                   "Line ~a: expected THEN SET variable = integer"
                   then-line))

          (define action-variable
            (string-downcase (third then-tokens)))

          (define action-value
            (parse-integer (fifth then-tokens)))

          (define parsed-rule
            (rule name
                  variable
                  operator
                  threshold
                  action-variable
                  action-value))

          ;; Пропускаем сразу три строки правила
          (loop (drop remaining 3)
                assignments
                (cons parsed-rule rules))]

         [else
          (error 'parser
                 "Line ~a: unknown statement ~a"
                 line-number
                 (first tokens))])])))

;; ==========================================
;; 7. Исполнение присваиваний
;; ==========================================

(define (set-variable! name value)
  (hash-set! env name value)
  (printf "  ~a = ~a\n" name value))

(define (get-variable name)
  (hash-ref env name
            (lambda ()
              (error 'runtime "Unknown variable: ~a" name))))

;; ==========================================
;; 8. Вычисление условий
;; ==========================================

(define (evaluate-condition r)
  (define value
    (get-variable (rule-variable r)))

  (define threshold
    (rule-threshold r))

  (define operator
    (rule-operator r))

  (cond
    [(string=? operator ">=") (>= value threshold)]
    [(string=? operator "<=") (<= value threshold)]
    [(string=? operator ">")  (> value threshold)]
    [(string=? operator "<")  (< value threshold)]
    [(string=? operator "=")  (= value threshold)]
    [else (error 'runtime "Unknown operator: ~a" operator)]))

;; ==========================================
;; 9. Исполнение правил
;; ==========================================

(define (execute-rule! r)
  (printf "\nChecking rule: ~a\n" (rule-name r))

  (if (evaluate-condition r)
      (begin
        (displayln "  MATCH")
        (set-variable!
         (rule-action-variable r)
         (rule-action-value r)))
      (displayln "  SKIP")))

;; ==========================================
;; 10. Главная функция
;; ==========================================

(define (main)
  (displayln "=== Parsing DSL ===")

  (define-values (assignments rules)
    (parse-dsl source))

  (printf "Parsed assignments: ~a\n" (length assignments))
  (printf "Parsed rules: ~a\n" (length rules))

  (displayln "\n=== Initial state ===")

  (for ([a (in-list assignments)])
    (set-variable! (assignment-name a)
                   (assignment-value a)))

  (displayln "\n=== Execute rules ===")

  (for ([r (in-list rules)])
    (execute-rule! r))

  (displayln "\n=== Final state ===")

  (for ([name (in-list (sort (hash-keys env) string<?))])
    (printf "~a = ~a\n" name (get-variable name))))

(main)
