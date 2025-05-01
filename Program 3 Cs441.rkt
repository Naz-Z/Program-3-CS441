#lang racket

;; === Data structures and helpers ===

(struct success (value) #:transparent)
(struct failure (message) #:transparent)

(define (from-success v x)
  (if (success? v) (success-value v) x))

(define (from-failure v x)
  (if (failure? v) (failure-message v) x))

;; The environment (state) is a list of (name . value)

(define empty-state '())

(define (lookup id state)
  (cond [(assoc id state) => (lambda (p)
                                (let ([val (cdr p)])
                                  (if (equal? val 'undefined)
                                      (failure (format "Error: ~a is undefined" id))
                                      (success val))))]
        [else (failure (format "Error: ~a not found" id))]))

(define (add-variable id value state)
  (if (assoc id state)
      (failure (format "Error: ~a already defined" id))
      (success (cons (cons id value) state))))

(define (assign-variable id value state)
  (if (assoc id state)
      (success (map (lambda (pair)
                      (if (equal? (car pair) id)
                          (cons id value)
                          pair))
                    state))
      (failure (format "Error: ~a is not defined" id))))

(define (remove-variable id state)
  (if (assoc id state)
      (remove* (list id) state car)
      (begin (printf "Error: remove ~a: variable not defined, ignoring\n" id) state)))

;; === Evaluator ===

(define (eval-expr expr state)
  (cond
    [(number? expr) (success (list expr state))]
    [(symbol? expr) (failure (format "Error: unexpected symbol ~a" expr))]
    [(not (list? expr)) (failure (format "Error: invalid expression ~a" expr))]
    [else
     (match expr
       [(list 'num n) (success (list n state))]
       [(list 'id id) (match (lookup id state)
                        [(success v) (success (list v state))]
                        [(failure msg) (failure msg)])]
       [(list 'add e1 e2)
        (eval-binary-op + e1 e2 state)]
       [(list 'sub e1 e2)
        (eval-binary-op - e1 e2 state)]
       [(list 'mult e1 e2)
        (eval-binary-op * e1 e2 state)]
       [(list 'div e1 e2)
        (let ([res (eval-binary-op / e1 e2 state)])
          (if (and (success? res) (= (car (success-value res)) 0))
              (failure "Error: division by zero")
              res))]
       [(list 'define id)
        (add-variable id 'undefined state)]
       [(list 'define id expr)
        (match (eval-expr expr state)
          [(success (list v new-state)) (match (add-variable id v new-state)
                                          [(success new-state2) (success (list v new-state2))]
                                          [(failure msg) (failure msg)])]
          [(failure msg) (failure msg)])]
       [(list 'assign id expr)
        (match (eval-expr expr state)
          [(success (list v new-state)) (match (assign-variable id v new-state)
                                          [(success updated-state) (success (list v updated-state))]
                                          [(failure msg) (failure msg)])]
          [(failure msg) (failure msg)])]
       [(list 'remove id)
        (success (list (format "Removed ~a" id) (remove-variable id state)))]
       [_ (failure (format "Error: unknown operation ~a" expr))])]))

(define (eval-binary-op op e1 e2 state)
  (match (eval-expr e1 state)
    [(success (list v1 state1))
     (match (eval-expr e2 state1)
       [(success (list v2 state2))
        (success (list (op v1 v2) state2))]
       [(failure msg) (failure msg)])]
    [(failure msg) (failure msg)]))

;; === REPL ===

(define (repl state)
  (display "Enter expression (or 'quit' to exit): ")
  (flush-output)
  (let ([input (read)])
    (cond
      [(equal? input 'quit) (printf "Goodbye!\n")]
      [else
       (let ([result (eval-expr input state)])
         (cond
           [(success? result)
            (let ([val (car (success-value result))]
                  [new-state (cadr (success-value result))])
              (printf "Result: ~a\n" val)
              (repl new-state))]
           [(failure? result)
            (printf "Failure: ~a\n" (failure-message result))
            (repl state)]))])))

;; === Start Program ===

(repl empty-state)
