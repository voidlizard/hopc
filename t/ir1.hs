
($ foreign "getint" get-int ((:unit) :int) $)
($ foreign "plus"   + ((:int :int) :int) $)


(define (f1) (+ (get-int) (get-int)))

(define (f2) f1)

(f1)

(f2)


