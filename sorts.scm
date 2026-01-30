;; util
(define with (lambda (s f) (apply f s)))

;;; \Theta(n^2)  ---  at least imperatively
;; bubble-sort
(define bubble-sort
  (letrec ((bub (lambda (a b . s)                  
                    (if (< b a)                      
                        `(,b ,a ,@s)                      
                        `(,a ,b ,@s))))
           (bubble-n (lambda (s n)                         
                         (if (or (null? s) (null? (cdr s)) (zero? n))                             
                             s
                             (with (with s bub)
                                   (lambda (a . s)
                                        (cons a (bubble-n s (- n 1))))))))
           (bubble-sort (lambda (s n)                          
                            (if (zero? n)                              
                                s                              
                                (bubble-sort (bubble-n s n) (- n 1)))))
           )
    (lambda (s) (bubble-sort s (length s)))
    ))

;; insertion-srot
(define insertion-sort
  (letrec ((insert (lambda (n sorted f)
                     (if (null? sorted)
                         (apply f `(,n))
                         (with sorted (lambda (a . rest)
                                        (insert n rest (lambda (n . rest)
                                                         (if (< n a)
                                                             (apply f `(,n ,a ,@rest))
                                                             (apply f `(,a ,n ,@rest))))))))))
           (isort (lambda (sorted s)
                    (if (null? s)
                        sorted
                        (with s (lambda (n . s)
                                  (insert n sorted (lambda sorted
                                                     (isort sorted s)))))))))
    (lambda (s) (isort '() s))))

;; max-sort
(define max-sort
  (letrec ((get-max (lambda (s m f)
                      (if (null? s)
                          (f s m)
                          (with s (lambda (a . s)
                                    (if (> a m)
                                        (get-max s a (lambda (s max)
                                                       (f (cons m s) max)))
                                        (get-max s m (lambda (s max)
                                                       (f (cons a s) max)))))))))
           (max-sort (lambda (s sorted)
                       (if (null? s)
                           sorted
                           (with s (lambda (a . s)
                                     (get-max s a (lambda (s max)
                                                    (max-sort s (cons max sorted))))))))))
    (lambda (s) (max-sort s '()))))

;;; \Theta(nlogn)
;; merge-sort
(define merge-sort
  (letrec ((split (lambda (s f)
                    (if (or (null? s) (null? (cdr s)))
                        (f s '())
                        (with s (lambda (a b . s)
                                  (split s (lambda (s1 s2)
                                             (f (cons a s1) (cons b s2)))))))))
           (merge (lambda (s1 s2)
                    (cond ((null? s1) s2)
                          ((null? s2) s1)
                          (else (with s1 (lambda (a1 . ss1)
                                           (with s2 (lambda (a2 . ss2)
                                                      (if (< a1 a2)
                                                          (cons a1 (merge ss1 s2))
                                                          (cons a2 (merge s1 ss2)))))))))))
           (msort (lambda (s)
                    (if (or (null? s) (null? (cdr s)))
                        s
                        (split s (lambda (s1 s2)
                                   (merge (msort s1) (msort s2))))))))
    msort
    ))

;; quick-sort
(define quick-sort
  (letrec ((split-by-pivot (lambda (p s f)
                             (if (null? s)
                                 (f '() '())
                                 (with s (lambda (a . s)
                                           (if (< a p)
                                               (split-by-pivot p s (lambda (lt gt)
                                                                     (f (cons a lt) gt)))
                                               (split-by-pivot p s (lambda (lt gt)
                                                                     (f lt (cons a gt))))))))))
           (qsort (lambda (s)
                    (if (null? s)
                        s
                        (with s (lambda (p . s)
                                  (split-by-pivot p s (lambda (lt gt) 
                                                        `(,@(qsort lt) ,p ,@(qsort gt))))))))))
    qsort
    ))


;;; non-comparing sorts
;; counting-sort
(define counting-sort
  (letrec ((add-to-histogram (lambda (n hist)
                               (if (zero? n)
                                   (if (null? hist)
                                       '(1)
                                       (with hist (lambda (curr . rest)
                                                    (cons (+ curr 1) rest))))
                                   (if (null? hist)
                                       (cons 0 (add-to-histogram (- n 1) '()))
                                       (with hist (lambda (curr . rest)
                                                    (cons curr (add-to-histogram (- n 1) rest))))))))
           (sorted-from-histogram (lambda (i hist)
                                    (if (null? hist)
                                        '()
                                        (with hist (lambda (curr . rest)
                                                     (if (zero? curr)
                                                         (sorted-from-histogram (+ i 1) rest)
                                                         (cons i (sorted-from-histogram i (cons (- curr 1) rest)))))))))
           (c-sort (lambda (s hist)
                     (if (null? s)
                         (sorted-from-histogram 0 hist)
                         (with s (lambda (a . s)
                                   (c-sort s (add-to-histogram a hist))))))))
    (lambda (s) (c-sort s '()))))

