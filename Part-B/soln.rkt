#lang racket
(provide (all-defined-out)) 

;; ------------------------------------------------------------
;;1
(define (sequence low high s)
  (if (> low high)
      null
      (cons low (sequence (+ low s) high s))))

;; ------------------------------------------------------------
;2

(define (string-append-map xs suffix)
  (map (lambda (word) (string-append word suffix)) xs))

; ------------------------------------------------------------
;3

(define (list-nth-mod xs n)
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (car (list-tail
              xs (remainder n (length xs))))]))

;; ------------------------------------------------------------
; 4

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([x (s)])
        (cons (car x) (stream-for-n-steps (cdr x) (- n 1))))))

;; ------------------------------------------------------------
; 5

(define funny-number-stream
  (letrec ([f (lambda (x) (cons
                           (if (= (remainder x 5) 0)
                               (* -1 x)
                               x)
                           (lambda() (f (+ x 1)))))])
    (lambda () (f 1))))

;; ------------------------------------------------------------
;6

(define dan-then-dog
  (letrec ([f (lambda (x) (cons x
                                (if (string=? x "dan.jpg")
                                    (lambda () (f "dog.jpg"))
                                    (lambda () (f "dan.jpg")))))])
    (lambda() (f "dan.jpg"))))

;; ------------------------------------------------------------
;7

(define (stream-add-zero st)
  (letrec ([f (lambda (st)
                (let ([v (st)])
                  (cons (cons 0 (car v))
                        (lambda () (f (cdr v))))))])
    (lambda () (f st))))

;; ------------------------------------------------------------
; 8

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons
                       (list-nth-mod xs n)
                       (list-nth-mod ys n))
                      (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;; ------------------------------------------------------------
;9
(define (vector-assoc v vec)
  (letrec ([f (lambda(n)
                (cond
                  [(= (vector-length vec) 0) #f]
                  [(not (pair? (vector-ref vec n))) (f (+ n 1))]
                  [#t (if (equal? (car (vector-ref vec n)) v)
                          (vector-ref vec n)
                          (f (+ n 1)))]))])
    (f 0)))

;; ----------------------------10--------------------------------
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [current-pos 0]
           [check-cache (lambda (v x)
                          (cond
                            [(equal? (vector-ref cache x) #f) #f]
                            [(equal? (car (vector-ref cache x)) v)
                             (vector-ref cache x)]
                            [#f (check-cache v (+ x 1))]
                          ))]
           [f (lambda (v)
                (let ([cache-ret-val (check-cache v 0)])
                  (if cache-ret-val
                      cache-ret-val
                      (begin
                        (vector-set! cache current-pos (assoc v xs))
                        (if (= n (- current-pos))
                            (set! current-pos 0)
                            (set! current-pos (+ current-pos 1)))
                        (vector-ref cache (- current-pos 1))))))])
    f))

;; ------------------------------CHALLANGE PROBLEM-----------------------

(define-syntax while-less
  (syntax-rules (do)
    [(while-less limit do e2)
     (letrec ([limit-val limit]
              [loop (lambda ()
                     (if (>= e2 limit-val)
                         #t
                         (loop)))])
       (loop))]))

       


                
                

    
