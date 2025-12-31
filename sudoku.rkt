;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))



;; A (matrixof X) is a (listof (listof X))
;; Requires: all (listof X) have the same length.

;;
;;question 1, part a
;;
;; all-satisfy? is a function that produces true if every value in the matrix satisfies the given pred
(define (all-satisfy? pred matrix)
  (local [(define (row-works cur-row)
            (foldr (lambda (x sofar) (and (pred x) sofar)) true cur-row))]
    (foldr (lambda (row sofar) (and (row-works row) sofar)) true matrix)))

;;
;;question 1, part b
;;
;; any-satisfy? checks to see if any value satisfies the given pred
(define (any-satisfy? pred matrix)
  (not (all-satisfy? (lambda (x) (not (pred x))) matrix)))

;;
;;question 1, part c
;;
;;

(define (find-where pred matrix)
  (local [(define (row-scanner row row-index col-index acc)
       (cond [(list? acc) acc]                            
         [(empty? row) false]                           
         [(pred (first row)) (list col-index row-index)] 
         [else (row-scanner (rest row) row-index (add1 col-index) acc)]))
     (define (scan-rows rows row-index acc)
       (cond [(list? acc) acc]                              
         [(empty? rows) false]                           
         [else (scan-rows (rest rows) (add1 row-index)
                          (row-scanner (first rows) row-index 0 acc))]))]
    (scan-rows matrix 0 false)))



;;
;;question 2
;;
;; 
(define (strings->puzzle strs)
  (map (lambda (s) (local [(define row-size (string-length s))
       (define (char->cell c)
          (cond [(char=? c #\?) (build-list row-size add1)]
            [true (list (- (char->integer c) 48))]))]
       (map char->cell (string->list s)))) strs))

;;
;;question 3
;;
;;
;; Removes single values by filling them in and removing from row/column
;; repeats until no singles remain

(define (remove-singles puzzle)
  (local [(define (single? cell) (and (list? cell) (= (length cell) 1)))
          (define (has-single? board) (any-satisfy? single? board))
          (define (first-single-coords board) (find-where single? board))
          (define (first-single-value board)
              (local [(define coords (first-single-coords board))
               (define col (first coords))
               (define row (second coords))]
               (first (get-cell board col row))))
     (define (get-cell board col row)
       (cond [(zero? row)
             (cond [(zero? col) (first (first board))]
                   [else (get-cell (map rest board) (sub1 col) 0)])]
                   [else (get-cell (rest board) col (sub1 row))]))
     (define (set-cell board col row val)
       (local [(define (update-row r col-idx)
                 (cond [(empty? r) empty]
                   [(zero? col-idx) (cons val (rest r))]
                   [else (cons (first r) (update-row (rest r) (sub1 col-idx)))]))
               (define (update-rows rows row-idx)
                 (cond [(empty? rows) empty]
                       [(zero? row-idx) (cons (update-row (first rows) col) (rest rows))]
                       [else (cons (first rows) (update-rows (rest rows) (sub1 row-idx)))]))]
         (update-rows board row)))
     (define (remove-from-cell cell val)
       (cond [(not (list? cell)) cell]
         [else (filter (lambda (v) (not (= v val))) cell)]))
     (define (remove-from-row board row-idx val ex-col ex-row)
       (local [(define (update-cell cell col-idx)
                 (cond [(and (= col-idx ex-col) (= row-idx ex-row)) cell]
                   [else (remove-from-cell cell val)]))
               (define (update-rows rows curr-row)
                 (cond [(empty? rows) empty]
                       [(zero? curr-row) (cons (map update-cell (first rows) 
                       (build-list (length (first rows)) identity)) (rest rows))]
                       [else (cons (first rows) (update-rows (rest rows) (sub1 curr-row)))]))]
         (update-rows board row-idx)))
     (define (remove-from-col board col-idx val ex-col ex-row)
       (local [(define (update-cell cell curr-col curr-row)
                 (cond [(and (= curr-col ex-col) (= curr-row ex-row)) cell]
                   [(= curr-col col-idx) (remove-from-cell cell val)]
                   [else cell]))
               (define (update-row row row-idx)
                 (map (lambda (cell col-idx) (update-cell cell col-idx row-idx)) row
                      (build-list (length row) identity)))]
         (map update-row board (build-list (length board) identity))))
     (define (one-single board)
       (local [(define coords (first-single-coords board))
               (define col (first coords))
               (define row (second coords))
               (define val (first-single-value board))
               (define puz1 (set-cell board col row val))
               (define puz2 (remove-from-row puz1 row val col row))
               (define puz3 (remove-from-col puz2 col val col row))]
         puz3))
     (define (solve board)
       (cond [(has-single? board) (solve (one-single board))]
         [else board]))]
    (solve puzzle)))


;;
;;question 4
;;
;;
;; solve-latin: (Solution -> Bool) Puzzle -> (anyof Solution empty)
;; Ignore the parameter; always produce true.
;; yes: Any -> Bool
(define (yes x) true)
;; Ignore the parameter; always produce false.
;; no: Any -> Bool
(define (no x) false)
;; Determine if the diagonal of p has a 2 in it.
;; diagonal-has-2?: Solution -> Bool
(define (diagonal-has-2? p)
(and (not (empty? p))
(or (= 2 (first (first p)))
(diagonal-has-2? (map rest (rest p))))))


(define (solve-latin pred puzzle)
  (local [(define simplified (remove-singles puzzle)) 
     (define (has-empty? table)
       (any-satisfy? (lambda (cell) (and (list? cell) (empty? cell))) table))
     (define (all-filled? table)
       (all-satisfy? (lambda (cell) (not (list? cell))) table))
     (define (find-first table)
       (local [(define (check-cell cell col row)
            (cond [(and (list? cell) (> (length cell) 1)) (list col row cell)]
              [else false]))
          (define (check-row row row-idx col-idx)
            (cond [(empty? row) false]
              [else (local [(define result (check-cell (first row) col-idx row-idx))]
                      (cond [(list? result) result]  
                            [else (check-row (rest row) row-idx (add1 col-idx))]))]))
          (define (check-rows rows row-idx)
            (cond [(empty? rows) false]
              [else (local [(define result (check-row (first rows) row-idx 0))]
                      (cond [(list? result) result] 
                            [else (check-rows (rest rows) (add1 row-idx))]))]))]
         (check-rows table 0)))   
     (define (set-cell table col row val)
       (local [(define (update-row r col-idx)
            (cond [(empty? r) empty]
              [(zero? col-idx) (cons (list val) (rest r))]
              [else (cons (first r) (update-row (rest r) (sub1 col-idx)))]))
          (define (update-rows rows row-idx)
            (cond [(empty? rows) empty]
              [(zero? row-idx) (cons (update-row (first rows) col) (rest rows))]
              [else (cons (first rows) (update-rows (rest rows) (sub1 row-idx)))]))]
         (update-rows table row)))
     (define (try-values table col row choices)
       (cond [(empty? choices) empty]
         [else (local [(define guess (set-cell table col row (first choices)))
                       (define result (solve-latin pred guess))]
                 (cond [(cons? result) result] 
                   [else (try-values table col row (rest choices))]))]))
     (define (solve table)
       (cond [(has-empty? table) empty]
         [(all-filled? table) (cond [(pred table) table]
                                   [else empty])]
         [else (local [(define choice (find-first table))]
                 (cond [(list? choice)
                 (local [(define col (first choice))
                         (define row (second choice))
                         (define vals (third choice))]
                         (try-values table col row vals))]
                   [else empty]))]))]
    (solve simplified)))

;;
;;question 5
;;
;;
(define (sudoku? solution)
  (local [(define (grab-subsquare sol row-start col-start)
       (local [
          (define (get-three-from-row row col-start)
            (cond
              [(zero? col-start) (list (first row) (second row) (third row))]
              [else (get-three-from-row (rest row) (sub1 col-start))]))
          (define (get-rows sol row-start)
            (cond
              [(zero? row-start) (list (first sol) (second sol) (third sol))]
              [else (get-rows (rest sol) (sub1 row-start))]))]
         (append (get-three-from-row (first (get-rows sol row-start)) col-start)
                 (get-three-from-row (second (get-rows sol row-start)) col-start)
                 (get-three-from-row (third (get-rows sol row-start)) col-start))))     
     (define (lists-equal? lst1 lst2)
       (cond [(and (empty? lst1) (empty? lst2)) true]
         [(or (empty? lst1) (empty? lst2)) false]
         [(= (first lst1) (first lst2)) (lists-equal? (rest lst1) (rest lst2))]
         [else false]))
     (define (valid-square subsquare)
       (local [(define sorted (sort subsquare <))]
         (lists-equal? sorted '(1 2 3 4 5 6 7 8 9))))
     (define (check-all-subsquares sol)
       (and (valid-square (grab-subsquare sol 0 0))
            (valid-square (grab-subsquare sol 0 3))
            (valid-square (grab-subsquare sol 0 6))
            (valid-square (grab-subsquare sol 3 0))
            (valid-square (grab-subsquare sol 3 3))
            (valid-square (grab-subsquare sol 3 6))
            (valid-square (grab-subsquare sol 6 0))
            (valid-square (grab-subsquare sol 6 3))
            (valid-square (grab-subsquare sol 6 6))))]
    (check-all-subsquares solution)))







;;**************************************************
;; Test Cases
;;**************************************************
(check-expect (sudoku? '((1 2 3 4 5 6 7 8 9)
(4 6 5 7 8 9 1 2 3)
(7 8 9 1 2 3 4 5 6)
(4 5 6 7 8 9 1 2 3)
(1 2 3 4 5 6 7 8 9)
(7 8 9 1 2 3 4 5 6)
(7 8 9 1 2 3 6 5 4)
(4 5 6 7 8 9 1 2 3)
(3 2 1 4 5 6 7 8 9))) true)

(check-expect (sudoku? '((1 2 3 4 5 6 7 8 9)
(4 5 6 7 8 9 1 2 3)
(7 8 9 1 2 3 4 5 6)
(4 5 6 7 8 9 1 2 3)
(1 2 3 4 5 6 7 8 9)
(7 8 9 1 2 3 4 5 6)
(7 8 9 1 2 3 4 5 7)
(4 5 6 7 8 9 1 2 3)
(1 2 3 4 5 6 7 8 9))) false)

(check-expect
 (sudoku? '((5 3 4 6 7 8 9 1 2)
            (6 7 2 1 9 5 3 4 8)
            (1 9 8 3 4 2 5 6 7)
            (8 5 9 7 6 1 4 2 3)
            (4 2 6 8 5 3 7 9 1)
            (7 1 3 9 2 4 8 5 6)
            (9 6 1 5 3 7 2 8 4)
            (2 8 7 4 1 9 6 3 5)
            (3 4 5 2 8 6 1 7 9)))
 true)

(check-expect
 (sudoku? '((8 2 7 1 5 4 3 9 6)
            (9 6 5 3 2 7 1 4 8)
            (3 4 1 6 8 9 7 5 2)
            (5 9 3 4 6 8 2 7 1)
            (4 7 2 9 1 3 6 8 5)
            (6 1 8 5 7 2 9 3 4)
            (1 5 4 2 3 6 8 9 7)
            (7 8 9 8 4 1 5 6 3)
            (2 3 6 7 9 5 4 1 8)))
 false)

(check-expect
 (sudoku? '((2 7 6 3 1 5 9 4 8)
            (8 5 4 9 6 2 7 1 3)
            (9 1 3 8 7 4 6 5 2)
            (5 9 1 4 3 6 8 2 7)
            (4 3 8 2 5 7 1 6 9)
            (7 6 2 1 8 9 3 9 4)
            (3 8 7 5 2 1 4 9 6)
            (6 4 5 7 9 3 2 8 1)
            (1 2 9 6 4 8 5 3 7)))
 false)

(check-expect
 (sudoku? '((5 3 4 6 7 8 9 1 2)
            (6 7 2 1 9 5 3 4 8)
            (1 9 8 3 4 2 5 6 7)
            (8 5 9 7 6 1 4 2 3)
            (4 2 6 8 5 3 7 9 1)
            (7 1 3 9 2 4 8 5 6)
            (9 6 1 5 3 7 2 8 4)
            (2 8 7 4 1 9 6 3 5)
            (2 4 5 2 8 6 1 7 9))) 
 false)

(check-expect
 (sudoku? '((1 2 3 4 5 6 7 8 9)
            (1 2 3 4 5 6 7 8 9)
            (1 2 3 4 5 6 7 8 9)
            (1 2 3 4 5 6 7 8 9)
            (1 2 3 4 5 6 7 8 9)
            (1 2 3 4 5 6 7 8 9)
            (1 2 3 4 5 6 7 8 9)
            (1 2 3 4 5 6 7 8 9)
            (1 2 3 4 5 6 7 8 9)))
 false)

(check-expect
 (sudoku? '((5 3 4 6 7 8 9 1 2)
            (6 7 2 1 9 5 3 4 8)
            (1 9 8 3 4 2 5 6 7)
            (8 5 9 7 6 1 4 2 3)
            (4 2 6 8 5 3 7 9 1)
            (7 1 3 9 2 4 8 5 6)
            (9 6 1 5 3 7 2 8 4)
            (2 8 7 4 1 9 6 3 5)
            (3 4 5 2 8 6 9 7 9)))
 false)

(define 23puzzle (strings->puzzle '("???"
"?3?"
"??2")))
(check-expect (solve-latin yes 23puzzle)
'((1 2 3)
(2 3 1)
(3 1 2)))
(define 324puzzle (strings->puzzle '("??3?"
"??2?"
"?4??"
"????")))
(check-expect (solve-latin yes 324puzzle)
'((1 2 3 4)
(4 1 2 3)
(3 4 1 2)
(2 3 4 1)))
(check-expect (solve-latin diagonal-has-2? 324puzzle)
'((1 2 3 4)
(4 3 2 1)
(2 4 1 3)
(3 1 4 2)))
(check-expect (solve-latin no 324puzzle) empty)


(check-expect (remove-singles (strings->puzzle '("??3?"
"??2?"
"?4??"
"????")))
'(( (1 2 4) (1 2) 3 (1 2 4) )
( (1 3 4) (1 3) 2 (1 3 4) )
( (2 3) 4 1 (2 3) )
( (1 2 3) (1 2 3) 4 (1 2 3) )))


(check-expect (strings->puzzle '("???"
"?3?"
"??2"))
'(( (1 2 3) (1 2 3) (1 2 3) )
( (1 2 3) (3) (1 2 3) )
( (1 2 3) (1 2 3) (2) )))
(check-expect (strings->puzzle '("??3?"
"??2?"
"?4??"
"????"))
'(( (1 2 3 4) (1 2 3 4) (3) (1 2 3 4) )
( (1 2 3 4) (1 2 3 4) (2) (1 2 3 4) )
( (1 2 3 4) (4) (1 2 3 4) (1 2 3 4) )
( (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) )))

(define wherematrix '(( 1 2 3 4 )
( 4 5 (3 6) (1 2) )
( (7) 8 9 () )))
(check-expect (find-where list? wherematrix) '(2 1))
(check-expect (find-where empty? wherematrix) '(3 2))
(check-expect (find-where integer? wherematrix) '(0 0))

(check-expect (any-satisfy? symbol? '((2 3 4) (5 6 7)))
false)
(check-expect (any-satisfy? symbol? '((2 3 4) (5 six 7)))
true)

(check-expect (all-satisfy? integer? '((2 3 4) (5 6 7)))
true)
(check-expect (all-satisfy? integer? '((2 3 4) (5 six 7)))
false)
;; all integers → true
(check-expect
 (all-satisfy? integer? '((1 2 3) (4 5 6)))
 true)

;; one non-integer → false
(check-expect
 (all-satisfy? integer? '((1 2 x) (4 5 6)))
 false)

;; predicate symbol? → true
(check-expect
 (all-satisfy? symbol? '((a b c) (d e f)))
 true)

;; predicate symbol? → false
(check-expect
 (all-satisfy? symbol? '((a b c) (d 3 f)))
 false)

;; predicate even? → true
(check-expect
 (all-satisfy? even? '((2 4) (6 8) (10 12)))
 true)

;; predicate even? → false
(check-expect
 (all-satisfy? even? '((2 4) (6 7)))
 false)



;; empty matrix → vacuously true
(check-expect
 (all-satisfy? integer? '())
 true)

;; matrix with empty rows → vacuously true
(check-expect
 (all-satisfy? integer? '(() ()))
 true)

;; row is empty but others are valid → true
(check-expect
 (all-satisfy? integer? '(() (1 2)))
 true)

;; row is empty but another row contains failure → false
(check-expect
 (all-satisfy? integer? '(() (1 x)))
 false)



;; failure in first row
(check-expect
 (all-satisfy? integer? '((1 x 3) (4 5 6)))
 false)

;; failure in last row
(check-expect
 (all-satisfy? integer? '((1 2 3) (4 5 x)))
 false)

;; failure in the last element of the last row
(check-expect
 (all-satisfy? integer? '((1 2 3) (4 5 6) (7 8 x)))
 false)



;; number? checks both integers & reals → true
(check-expect
 (all-satisfy? number? '((3 4.5) (0 -2) (9/2 10)))
 true)

;; number? with a symbol → false
(check-expect
 (all-satisfy? number? '((3 4.5) (0 hi)))
 false)

(check-expect
 (any-satisfy? symbol? '((1 2 3) (4 5 6)))
 false)

(check-expect
 (any-satisfy? symbol? '((1 2 3) (4 six 6)))
 true)

(check-expect
 (any-satisfy? number? '((a b) (3 c)))
 true)

(check-expect
 (any-satisfy? number? '((a b) (c d)))
 false)

(check-expect
 (any-satisfy? even? '((1 3 5) (7 9 10)))
 true)

(check-expect
 (any-satisfy? even? '((1 3 5) (7 9 11)))
 false)

(check-expect
 (any-satisfy? integer? '())
 false)

(check-expect
 (any-satisfy? integer? '(() ()))
 false)

(check-expect
 (any-satisfy? integer? '(() (a b) (4)))
 true)

(check-expect
 (any-satisfy? integer? '(() (x y z)))
 false)

(check-expect
 (any-satisfy? symbol? '((a b c) (1 2 3)))
 true)

(check-expect
 (any-satisfy? symbol? '((1 2 3) (4 5 six)))
 true)

(check-expect
 (any-satisfy? integer? '((a b c) (d e 6)))
 true)

(check-expect
 (any-satisfy? string? '(("hi") (3 4)))
 true)

(check-expect
 (any-satisfy? string? '((1 2) (3 4)))
 false)

(check-expect
 (any-satisfy? char? '((1 2 #\c) (3 4)))
 true)

(check-expect
 (any-satisfy? char? '((1 2) (3 4)))
 false)

(check-expect
 (any-satisfy? rational? '((a b) (c 1/2)))
 true)


(check-expect
 (any-satisfy? integer? '((1.1 2.2) (3.3)))
 false)

;; first even? element
(define even-matrix '((1 3) (5 8) (7 9)))
(check-expect (find-where even? even-matrix) '(1 1))

;; matrix with only one row
(define single-row '((3 4 5)))
(check-expect (find-where even? single-row) '(1 0))

;; matrix with only one column
(define single-col '((1) (2) (3)))
(check-expect (find-where even? single-col) '(0 1))

;; matrix with empty rows
(define empty-rows '(() () (4 5)))
(check-expect (find-where integer? empty-rows) '(0 2))

;; matrix where first row has the match at the last column
(define last-col '((1 3 6) (7 8 9)))
(check-expect (find-where even? last-col) '(2 0))

;; matrix with first row empty and match in second row
(define first-row-empty '(() (2 3)))
(check-expect (find-where even? first-row-empty) '(0 1))

;; no matching element → should return false
(define nomatch '((a b) (c d)))
(check-expect (find-where integer? nomatch) false)

;; Single-digit row
(check-expect (strings->puzzle '("5"))
              '(((5))))

;; All question marks
(check-expect (strings->puzzle (list "???"))
              '(((1 2 3) (1 2 3) (1 2 3))))

;; Single row, mixed digits and "?"
(check-expect (strings->puzzle '("?4?"))
              '(((1 2 3) (4) (1 2 3))))

;; Multiple rows, all digits
(check-expect (strings->puzzle '("12"
                                 "34"))
              '(((1) (2))
                ((3) (4))))

;; Single column, mixed digits and "?"
(check-expect (strings->puzzle '("?" "3" "?"))
              (list
 (list (list 1))
 (list (list 3))
 (list (list 1))))

;; Maximum allowed size (9x9), all "?"
(define nine-q (build-list 9 (lambda (_) "?????????")))
(check-expect (strings->puzzle nine-q)
              (build-list 9 (lambda (_) (build-list 9 (lambda (_) (build-list 9 add1))))))


;; Digits at edges
(check-expect (strings->puzzle '("1?9"
                                 "?5?"
                                 "8?2"))
              '(((1) (1 2 3) (9))
                ((1 2 3) (5) (1 2 3))
                ((8) (1 2 3) (2))))

;; ==================================================
;; COMPREHENSIVE TEST CASES FOR remove-singles
;; ==================================================

;; Empty puzzle
(check-expect (remove-singles '())
              '())

;; Single cell with single value - unwraps to number
(check-expect (remove-singles '(((5))))
              '((5)))

;; Single cell with multiple values - no singles, unchanged
(check-expect (remove-singles '(((1 2 3))))
              '(((1 2 3))))

;; 1x2 puzzle with one single
;; (1) unwraps to 1, removes 1 from (1 2) → (2) which then unwraps
(check-expect (remove-singles '(((1) (1 2))))
              '((1 2)))

;; ----------------
;; NO SINGLES PRESENT
;; ----------------

;; 2x2 with no singles - completely unchanged
(check-expect (remove-singles '(((1 2) (1 2))
                                ((1 2) (1 2))))
              '(((1 2) (1 2))
                ((1 2) (1 2))))

;; 3x3 with no singles - completely unchanged
(check-expect (remove-singles '(((1 2 3) (1 2 3) (1 2 3))
                                ((1 2 3) (1 2 3) (1 2 3))
                                ((1 2 3) (1 2 3) (1 2 3))))
              '(((1 2 3) (1 2 3) (1 2 3))
                ((1 2 3) (1 2 3) (1 2 3))
                ((1 2 3) (1 2 3) (1 2 3))))

;; ----------------
;; ALREADY FULLY SOLVED
;; ----------------

;; 2x2 all singles - unwraps all
(check-expect (remove-singles '(((1) (2))
                                ((2) (1))))
              '((1 2) (2 1)))

;; 3x3 all singles - unwraps all
(check-expect (remove-singles '(((1) (2) (3))
                                ((2) (3) (1))
                                ((3) (1) (2))))
              '((1 2 3) (2 3 1) (3 1 2)))

;; ----------------
;; SINGLE CELL SINGLE VALUE
;; ----------------

;; One single in corner (0,0)
;; (1) at (0,0) unwraps to 1, removes 1 from row 0 and col 0
(check-expect (remove-singles '(((1) (1 2 3) (1 2 3))
                                ((1 2 3) (1 2 3) (1 2 3))
                                ((1 2 3) (1 2 3) (1 2 3))))
              '((1 (2 3) (2 3))
                ((2 3) (1 2 3) (1 2 3))
                ((2 3) (1 2 3) (1 2 3))))

;; One single in corner (2,0)
(check-expect (remove-singles '(((1 2 3) (1 2 3) (1))
                                ((1 2 3) (1 2 3) (1 2 3))
                                ((1 2 3) (1 2 3) (1 2 3))))
              '(((2 3) (2 3) 1)
                ((1 2 3) (1 2 3) (2 3))
                ((1 2 3) (1 2 3) (2 3))))

;; One single in corner (2,2)
(check-expect (remove-singles '(((1 2 3) (1 2 3) (1 2 3))
                                ((1 2 3) (1 2 3) (1 2 3))
                                ((1 2 3) (1 2 3) (1))))
              '(((1 2 3) (1 2 3) (2 3))
                ((1 2 3) (1 2 3) (2 3))
                ((2 3) (2 3) 1)))

;; One single in center of 3x3
(check-expect (remove-singles '(((1 2 3) (1 2 3) (1 2 3))
                                ((1 2 3) (2) (1 2 3))
                                ((1 2 3) (1 2 3) (1 2 3))))
              '(((1 2 3) (1 3) (1 2 3))
                ((1 3) 2 (1 3))
                ((1 2 3) (1 3) (1 2 3))))

;; ----------------
;; CASCADING SINGLES - SIMPLE
;; ----------------

;; Cascading: filling one creates another
;; Start: (2) at (1,1)
;; After filling (2): (1,0) becomes (1)
;; After filling (1): (1,2) becomes (3)
;; Total solution achieved
(check-expect (remove-singles '(((1 2) (1 2) (3))
                                ((1 2 3) (2) (1 2 3))
                                ((1 2 3) (1 2 3) (1 2 3))))
              '((2 1 3)
                (3 2 1)
                (1 3 2)))

;; Cascading in 2x2
;; (1) at (0,0) creates (2) at (1,0), then (1) at (0,1), then (2) at (1,1)
(check-expect (remove-singles '(((1) (1 2))
                                ((1 2) (1 2))))
              '((1 2)
                (2 1)))

;; ----------------
;; MULTIPLE NON-CASCADING SINGLES
;; ----------------

;; Two singles in different rows/columns, no cascade
(check-expect (remove-singles '(((1) (1 2 3) (1 2 3))
                                ((2 1 3) (2) (1 2 3))
                                ((2 1 3) (1 2 3) (1 2 3))))
              '((1 3 2)
                (3 2 1)
                (2 1 3)))

;; Three singles on diagonal
(check-expect (remove-singles '(((1) (1 2 3) (1 2 3))
                                ((1 2 3) (2) (1 2 3))
                                ((1 2 3) (1 2 3) (3))))
              '((1 3 2)
                (3 2 1)
                (2 1 3)))

;; ----------------
;; PARTIALLY FILLED PUZZLES
;; ----------------

;; Mix of already-filled numbers and singles
;; 1 is already filled (not a list), (2) is a single
(check-expect (remove-singles '(((1) (2 1 3) (2 1 3))
                                ((2 1 3) (2) (1 2 3))
                                ((2 1 3) (1 2 3) (1 2 3))))
              '((1 3 2)
                (3 2 1)
                (2 1 3)))

;; Mostly filled with a few singles
(check-expect (remove-singles '((1 2 (3))
                                (3 (1) 2)
                                (2 3 1)))
              '((1 2 3)
                (3 1 2)
                (2 3 1)))

;; ----------------
;; LARGER PUZZLES
;; ----------------

;; 4x4 with one single in corner
(check-expect (remove-singles '(((1) (1 2 3 4) (1 2 3 4) (1 2 3 4))
                                ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))
                                ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))
                                ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))))
              '((1 (2 3 4) (2 3 4) (2 3 4))
                ((2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))
                ((2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))
                ((2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))))

;; 4x4 with two singles that don't cascade
(check-expect (remove-singles '(((1) (1 2 3 4) (1 2 3 4) (1 2 3 4))
                                ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))
                                ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))
                                ((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))))
              '((1 (2 3 4) (2 3 4) (2 3 4))
                ((2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))
                ((2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))
                ((2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))))

;; 5x5 with single in middle
(check-expect (remove-singles 
               '(((1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5))
                 ((1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5))
                 ((1 2 3 4 5) (1 2 3 4 5) (3) (1 2 3 4 5) (1 2 3 4 5))
                 ((1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5))
                 ((1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5))))
              '(((1 2 3 4 5) (1 2 3 4 5) (1 2 4 5) (1 2 3 4 5) (1 2 3 4 5))
                ((1 2 3 4 5) (1 2 3 4 5) (1 2 4 5) (1 2 3 4 5) (1 2 3 4 5))
                ((1 2 4 5) (1 2 4 5) 3 (1 2 4 5) (1 2 4 5))
                ((1 2 3 4 5) (1 2 3 4 5) (1 2 4 5) (1 2 3 4 5) (1 2 3 4 5))
                ((1 2 3 4 5) (1 2 3 4 5) (1 2 4 5) (1 2 3 4 5) (1 2 3 4 5))))

;; ----------------
;; COMPLEX CASCADING
;; ----------------

;; Multiple cascading stages - fully solves 3x3
;; This goes through many stages of creating new singles
(check-expect (remove-singles '(((1 2 3) (1 2) (1 2 3))
                                ((1 2 3) (3) (1 2 3))
                                ((1 2 3) (1 2 3) (2))))
              '((1 2 3)
                (2 3 1)
                (3 1 2)))

;; Row of singles - all unwrap immediately
(check-expect (remove-singles '(((1) (2) (3))
                                ((2 3) (1 3) (1 2))
                                ((2 3) (1 3) (1 2))))
              '((1 2 3)
                ((2 3) (1 3) (1 2))
                ((2 3) (1 3) (1 2))))

;; ----------------
;; EDGE CASES
;; ----------------

;; Single that doesn't affect anything (already constrained)
(check-expect (remove-singles '(((1) (2) (3))
                                ((2) (3) (1))
                                ((3) (1) (2))))
              '((1 2 3)
                (2 3 1)
                (3 1 2)))

;; All cells in a row are singles
(check-expect (remove-singles '(((1) (2) (3))
                                ((2 3 1) (1 2 3) (1 3 2))
                                ((1 2 3) (1 2 3) (1 3 2))))
              '((1 2 3)
                ((2 3) (1 3) (1 2))
                ((2 3) (1 3) (1 2))))



;; ----------------
;; SPECIAL PATTERNS
;; ----------------


;; Puzzle where order of processing matters
(check-expect (remove-singles '(((1 2) (1 2) (3))
                                ((1 2) (3) (1 2))
                                ((3) (1 2) (1 2))))
              '(((1 2) (1 2) 3)
                ((1 2) 3 (1 2))
                (3 (1 2) (1 2))))

;; Already solved puzzle
(check-expect (solve-latin yes (strings->puzzle '("123"
                                                   "231"
                                                   "312")))
              '((1 2 3)
                (2 3 1)
                (3 1 2)))

;; 2x2 simple puzzle
(check-expect (solve-latin yes (strings->puzzle '("??"
                                                   "??")))
              '((1 2)
                (2 1)))

;; 2x2 with one cell filled
(check-expect (solve-latin yes (strings->puzzle '("1?"
                                                   "??")))
              '((1 2)
                (2 1)))

;; Puzzle that requires backtracking
(check-expect (solve-latin yes (strings->puzzle '("???"
                                                   "???"
                                                   "???")))
              '((1 2 3)
                (2 3 1)
                (3 1 2)))

;; Puzzle with mostly filled cells
(check-expect (solve-latin yes (strings->puzzle '("12?"
                                                   "?31"
                                                   "3?2")))
              '((1 2 3)
                (2 3 1)
                (3 1 2)))

;; 4x4 puzzle that's mostly filled
(check-expect (solve-latin yes (strings->puzzle '("1234"
                                                   "41??"
                                                   "??1?"
                                                   "????")))
              '((1 2 3 4)
                (4 1 2 3)
                (3 4 1 2)
                (2 3 4 1)))

;; Test with custom predicate - first element must be 2
(define (first-is-2? p)
  (and (not (empty? p))
       (not (empty? (first p)))
       (= 2 (first (first p)))))

(check-expect (solve-latin first-is-2? (strings->puzzle '("???"
                                                          "???"
                                                          "???")))
              '((2 1 3)
                (1 3 2)
                (3 2 1)))

;; Test impossible puzzle scenario (if such exists)
;; This would need a puzzle that has no valid Latin square solution
;; For now, we test with the 'no' predicate which rejects all solutions

;; 1x1 trivial puzzle
(check-expect (solve-latin yes (strings->puzzle '("?")))
              '((1)))

;; 1x1 already solved
(check-expect (solve-latin yes (strings->puzzle '("1")))
              '((1)))

;; 5x5 puzzle (larger test)
(check-expect (solve-latin yes (strings->puzzle '("?????"
                                                   "?????"
                                                   "?????"
                                                   "?????"
                                                   "?????")))
              (list
 (list 1 2 3 4 5)
 (list 2 1 4 5 3)
 (list 3 4 5 1 2)
 (list 4 5 2 3 1)
 (list 5 3 1 2 4)))

;; 3x3 with specific constraint
(define (corner-is-1? p)
  (and (not (empty? p))
       (not (empty? (first p)))
       (= 1 (first (first p)))))

(check-expect (solve-latin corner-is-1? (strings->puzzle '("???"
                                                           "???"
                                                           "???")))
              '((1 2 3)
                (2 3 1)
                (3 1 2)))

;; Test diagonal-has-2? with 3x3
(check-expect (solve-latin diagonal-has-2? (strings->puzzle '("???"
                                                              "???"
                                                              "???")))
              (list
 (list 1 2 3)
 (list 2 3 1)
 (list 3 1 2)))


