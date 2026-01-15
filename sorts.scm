;; util
(define with (lambda (s f) (apply f s)))

;;; \Theta(n^2) 
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
