(define-syntax pointer-inc
  (ir-macro-transformer
   (lambda (exp inject compare)
     (let-values (((_ exports _) (##sys#module-exports (alist-ref 'lolevel ##sys#module-table))))
       (if (alist-ref 'pointer+ exports)
           `(pointer+ ,@(cdr exp))
           `(pointer-offset ,@(cdr exp)))))))
