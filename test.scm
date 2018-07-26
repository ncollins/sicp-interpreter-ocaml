(begin
	(define a (- 10 6 1)) ;; a = 3
	(define b 3)
	(define f (lambda (x y) (let ((k (+ a b))) (- (* x y) k))))
	(define c (f (+ 1 1) 100)) ;; b = 200 - 6 = 194
	(set! a 10)
	(define d (f 100 3)) ;; d = 300 - 13 = 287
	(if (equal? (+ c d) 481) (string matches!) 0)) ;; 194 + 287 = 481
