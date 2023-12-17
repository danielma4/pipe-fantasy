;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pipe-dreamv3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Task 7
; Run (pipe-fantasy GAME-DEMO) :)
(require 2htdp/image)
(require 2htdp/universe)

(define-struct pipe [top bot left right filled?])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A  #true for 
;; one of top, bot, left, right indicates an opening in that direction.
;;Examples:
(define PIPE-TB (make-pipe #true #true #false #false #f))
(define PIPE-LR (make-pipe #false #false #true #true #f))
(define PIPE-TR (make-pipe #true #false #false #true #f))
(define PIPE-TL (make-pipe #true #false #true #false #f))
(define PIPE-BR (make-pipe #false #true #false #true #f))
(define PIPE-BL (make-pipe #false #true #true #false #f))
(define PIPE-TBLR (make-pipe #true #true #true #true #f))
(define PIPE-SPL (make-pipe #f #f #t #f #f))
(define PIPE-SPR (make-pipe #f #f #f #t #f))
(define PIPE-SPT (make-pipe #t #f #f #f #f))
(define PIPE-SPB (make-pipe #f #t #f #f #f))

;;Template:
;;pipe-temp: Pipe-> ?
(define (pipes temp p)
  (... (pipe-top? p)...
       (pipe-bot? p)...
       (pipe-left? p)...
       (pipe-right? p)...))


(define ALL-PIPES (cons PIPE-TB (cons PIPE-LR (cons PIPE-TR (cons PIPE-TL (cons PIPE-BR (cons PIPE-BL(cons PIPE-TBLR empty))))))))

;; pipe->image: Pipe Integer Integer -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length
;; If filled? then draw the pipe with goo.
(define (pipe->image pipe tile-side-length pipe-width)
  (local
    [; make-tb-pipe : Number -> Image
     (define (make-tb-pipe w)
       (rectangle w tile-side-length "solid" (if (pipe-filled? pipe) "green" "black")))
     ; make-lr pipe : Number -> Image
     (define (make-lr-pipe w)
       (rectangle tile-side-length w "solid" (if (pipe-filled? pipe) "green" "black")))
     ; make-corner-pipe : Number String String -> Image 
     (define (make-corner-pipe w d1 d2)
       (overlay/align "middle" d1
                      (cond
                        [(or (string=? "top" d1) (string=? "bottom" d1))
                         (rectangle w (+ (/ tile-side-length 2) (/ pipe-width 2)) "solid" (if (pipe-filled? pipe) "green" "black"))]
                        [else (rectangle (/ tile-side-length 2) w "solid" (if (pipe-filled? pipe) "green" "black"))])
                      (overlay/align d2 "middle"
                                     (cond
                                       [(or (string=? "top" d2) (string=? "bottom" d2))
                                        (rectangle w (+ (/ tile-side-length 2) (/ pipe-width 2)) "solid" (if (pipe-filled? pipe) "green" "black"))]
                                       [else (rectangle (/ tile-side-length 2) w "solid" (if (pipe-filled? pipe) "green" "black"))])
                                     (make-tile tile-side-length))))
     ; make-tile : Number -> Image
     (define (make-tile sl)
       (square sl "solid" "grey"))
     ; make-starting-pipe : Number String -> Image
     (define (make-starting-pipe w d)
       (cond
         [(or (string=? "left" d) (string=? "right" d))
          (overlay/align d "middle"
                         (rectangle (+ (/ tile-side-length 2) (/ pipe-width 2)) w "solid" (if (pipe-filled? pipe) "green" "black"))
                         (make-tile tile-side-length))]
         [(or (string=? "top" d) (string=? "bottom" d))
          (overlay/align "middle" d
                         (rectangle w (+ (/ tile-side-length 2) (/ pipe-width 2)) "solid" (if (pipe-filled? pipe) "green" "black"))
                         (make-tile tile-side-length))]))]
    (cond
      [(pipe? pipe)
       (cond
         [(and (pipe-top pipe) (pipe-bot pipe) (not (pipe-left pipe)) (not (pipe-right pipe)))
          (overlay (make-tb-pipe pipe-width) (make-tile tile-side-length))]
         [(and (pipe-left pipe) (pipe-right pipe) (not (pipe-bot pipe)) (not (pipe-top pipe)))
          (overlay (make-lr-pipe pipe-width) (make-tile tile-side-length))]
         [(and (pipe-top pipe) (pipe-left pipe) (not (pipe-bot pipe)) (not (pipe-right pipe)))
          (make-corner-pipe pipe-width "top" "left")] 
         [(and (pipe-top pipe) (pipe-right pipe) (not (pipe-left pipe)) (not (pipe-bot pipe)))
          (make-corner-pipe pipe-width "top" "right")]
         [(and (pipe-bot pipe) (pipe-left pipe) (not (pipe-right pipe)) (not (pipe-top pipe)))
          (make-corner-pipe pipe-width "bottom" "left")]
         [(and (pipe-bot pipe) (pipe-right pipe) (not (pipe-left pipe)) (not (pipe-top pipe)))
          (make-corner-pipe pipe-width "bottom" "right")]
         [(and (pipe-bot pipe) (pipe-left pipe) (pipe-right pipe) (pipe-top pipe))
          (overlay (make-lr-pipe pipe-width) (make-tb-pipe pipe-width) (make-tile tile-side-length))]
         [(and (pipe-bot pipe) (not (pipe-left pipe)) (not (pipe-right pipe)) (not (pipe-top pipe)))
          (make-starting-pipe pipe-width "bottom")]
         [(and (not (pipe-bot pipe)) (pipe-left pipe) (not (pipe-right pipe)) (not (pipe-top pipe)))
          (make-starting-pipe pipe-width "left")]
         [(and (not (pipe-bot pipe)) (not (pipe-left pipe)) (pipe-right pipe) (not (pipe-top pipe)))
          (make-starting-pipe pipe-width "right")]
         [(and (not (pipe-bot pipe)) (not (pipe-left pipe)) (not (pipe-right pipe)) (pipe-top pipe))
          (make-starting-pipe pipe-width "top")])])))


(define-struct pipe-rc [pipe r c])
;; A Pipe-RC is a (make-pipe-rc PIPE Number Number Number)
;; Interpretation: A pipe with a given row and column.
;;Examples:
(define PIPE1 (make-pipe-rc PIPE-TB 4 5))
(define PIPE2 (make-pipe-rc PIPE-LR 2 4))
(define PIPE3 (make-pipe-rc PIPE-TB 6 1))
(define PIPE4 (make-pipe-rc PIPE-LR 2 5))
(define SP1 (make-pipe-rc PIPE-SPL 3 4))
(define SP2 (make-pipe-rc PIPE-SPB 5 2))
;Template:
;pipe-rc temp: pipe-rc -> ?
#;(define (pipe-rc temp p)
  (... (pipe-rc-pipe p)...
       (pipe-rc-r p)...
       (pipe-rc-c p)...))

; A Grid is one of:
; Number
; (make-pipe-rc pipe r c) 
; And represents the placed pipes on a grid
; the number represents n for an nxn grid

(define GRID-SIZE7 7)
(define GRID-SIZE8 8)
(define GRID-SIZE4 4)
(define STARTING-GRID (list GRID-SIZE7))
(define GRID-1 (list SP1 PIPE1 PIPE2 PIPE3 PIPE4 GRID-SIZE7))
(define GRID-2 (list SP2 PIPE1 PIPE2 GRID-SIZE4))

#;(define (grid-temp g)
  (cond
    [(empty? g) ...]
    [(cons? g) ...
     [cond
       [(number? (first g)) ...]
       [(pipe-rc? (first g)) ...]]]))


;; place-pipe: Grid Pipe Integer Integer Boolean -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.
(define (place-pipe grid pipe row col)
  (local [(define NEW-PIPE (make-pipe-rc pipe row col))]
    (cons NEW-PIPE grid)))
(check-expect (place-pipe GRID-2 PIPE-TB 0 0) (list (make-pipe-rc PIPE-TB 0 0) SP2 PIPE1 PIPE2 GRID-SIZE4))
(check-expect (place-pipe GRID-1 PIPE-LR 1 2) (list (make-pipe-rc PIPE-LR 1 2) SP1 PIPE1 PIPE2 PIPE3 PIPE4 GRID-SIZE7))
(check-expect (place-pipe GRID-2 PIPE-TBLR 3 0) (list (make-pipe-rc PIPE-TBLR 3 0) SP2 PIPE1 PIPE2 GRID-SIZE4))


;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.
(define (pipe-at grid row col)
  (cond
    [(number? (first grid)) #f]
    [(cons? grid)
     (if (and (= row (pipe-rc-r (first grid))) (= col (pipe-rc-c (first grid))))
         (pipe-rc-pipe (first grid))
         (pipe-at (rest grid) row col))]))
(check-expect (pipe-at GRID-1 4 5) PIPE-TB)
(check-expect (pipe-at GRID-2 0 0) #f)
(check-expect (pipe-at GRID-2 2 4) PIPE-LR)
(check-expect (pipe-at GRID-1 2 5) PIPE-LR)
(check-expect (pipe-at (list
                        (make-pipe-rc (make-pipe #false #true #false #true #true) 2 1)
                        (make-pipe-rc (make-pipe #true #true #true #true #true) 3 1)
                        (make-pipe-rc (make-pipe #true #false #false #true #true) 4 1)
                        (make-pipe-rc (make-pipe #true #false #true #false #true) 4 2)
                        (make-pipe-rc (make-pipe #true #true #false #false #true) 3 2)
                        (make-pipe-rc (make-pipe #true #true #true #true #true) 2 2)
                        (make-pipe-rc (make-pipe #false #true #true #false #true) 1 2)
                        (make-pipe-rc (make-pipe #false #false #false #true #true) 1 1)
                        6) 2 2) (make-pipe #t #t #t #t #t))

;; grid->image: Grid Integer Integer -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.
(define (grid->image grid tile-side-length pipe-width lop gs)
  (local [; get-n : grid -> Number
          (define (get-n g)
            (cond
              [(number? (first g)) (first g)]
              [else (get-n (rest g))]))
          (define n (get-n grid)) 
          ; pipe->grid: Pipe Integer Integer Integer Integer Boolean -> Image
          (define (pipe->grid pipe row col tile-side-length pipe-width n)
            (local [; x-coord : Number -> Number
                    (define (x-coord c)
                      (+ (* col tile-side-length) (/ tile-side-length 2)))
                    ; y-coord : Number -> Number
                    (define (y-coord r)
                      (+ (* row tile-side-length) (/ tile-side-length 2)))]
              (place-image (pipe->image pipe tile-side-length pipe-width) (x-coord col) (y-coord row) (make-grid n tile-side-length))))
          ; make-grid : Number -> Grid
          (define (make-grid n tile-side-length)
            (local [; sq : Number -> Image
                    (define (sq l)
                      (square tile-side-length "outline" "black"))
                    (define (num n)
                      (+ 1 1))
                    (define GRID-LIST (build-list (- n 1) sq))
                    (define NUM-LIST (build-list n num))
                    (define (build-grid loi lon)
                      (cond
                        [(empty? lon) empty-image]
                        [(cons? lon) 
                         (above (foldr beside (square tile-side-length "outline" "black") loi) (build-grid loi (rest lon)))]))]
              (build-grid GRID-LIST NUM-LIST)))
          ; incoming-pipes : [List-of Pipe] -> Image
          (define (incoming-pipes lop)
            (above (if (and (> (length lop) 0) (pipe? (first lop)))
                       (pipe->image (first lop) 40 10)
                       empty-image)
                   (if (and (> (length lop) 1) (pipe? (second lop)))
                       (pipe->image (second lop) 40 10)
                       empty-image)
                   (if (and (> (length lop) 2) (pipe? (third lop)))
                       (pipe->image (third lop) 40 10)
                       empty-image)
                   (if (and (> (length lop) 3) (pipe? (fourth lop)))
                       (pipe->image (fourth lop) 40 10)
                       empty-image)))
          ; grid->image-helper : Grid Number Number
          (define (grid->image-helper grid tile-side-length pipe-width)
            (cond
              [(number? (first grid)) (make-grid (first grid) tile-side-length)]
              [(cons? grid)
               (overlay (pipe->grid (pipe-rc-pipe (first grid))
                                    (pipe-rc-r (first grid))
                                    (pipe-rc-c (first grid))
                                    tile-side-length
                                    pipe-width
                                    n)
                        (grid->image-helper (rest grid) tile-side-length pipe-width))]))]
    (beside (grid->image-helper grid tile-side-length pipe-width) (above (text (number->string (get-score gs)) 25 "black") (incoming-pipes lop)))))



(define-struct goo-flow [row col dir])
; a GooFlow is a (make-goo-flow Number Number String)
; - row represents the row of the current goo
; - col represents the column of the current goo
; - dir represents the current direction of the goo flow (top, bot, left, right)
(define GOO-LEFT (make-goo-flow 3 4 "left"))
(define GOO-RIGHT (make-goo-flow 2 4 "right"))
(define GOO-TOP (make-goo-flow 4 4 "up"))
(define GOO-BOT (make-goo-flow 5 2 "down"))
#;(define (goo-flow-temp gf)
    (... (goo-flow-row gf) ...
         (goo-flow-col gf) ...
         (goo-flow-dir gf) ...))

(define-struct gamestate [grid lop sp gf num-replaced time-left])
; A GameState is (make-gamestate Grid [List-of Pipe] Pipe GooFlow)
; and represents the state of the Pipe Dream game
(define GAME1 (make-gamestate GRID-1 ALL-PIPES SP1 GOO-LEFT 0 140))
(define GAME2 (make-gamestate GRID-2 ALL-PIPES SP2 GOO-BOT 0 140))
(define GAME3 (make-gamestate (cons SP1 STARTING-GRID) ALL-PIPES SP1 GOO-LEFT 0 140))

#;(define (gamestate-temp gs)
    (... (grid-temp (gamestate-grid gs)) ...
         (pipe-temp (gamestate-lop)) ...))


;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState`
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.
(define (place-pipe-on-click gs x y m)
  (local [; get-rc : Number -> Number
          (define (get-rc a)
            (quotient a 40))
          ; not-same-pipe? : Pipe-rcOrNumber -> Boolean
          (define (not-same-pipe? pon)
            (cond
              [(number? pon) #t]
              [(pipe-rc? pon)
               (cond
                 [(and (= (get-rc y) (pipe-rc-r pon)) (= (get-rc x) (pipe-rc-c pon)))
                  #f]
                 [else #t])]))]
    (cond
      [(empty? (gamestate-lop gs)) gs]
       #;(cond
         [(mouse=? "button-up" m)
          (make-gamestate (place-pipe
                           (gamestate-grid gs)
                           (make-pipe (pipe-top (pipe-at (gamestate-grid gs) (goo-flow-row (gamestate-gf gs)) (goo-flow-col (gamestate-gf gs)))) 
                                      (pipe-bot (pipe-at (gamestate-grid gs) (goo-flow-row (gamestate-gf gs)) (goo-flow-col (gamestate-gf gs))))
                                      (pipe-left (pipe-at (gamestate-grid gs) (goo-flow-row (gamestate-gf gs)) (goo-flow-col (gamestate-gf gs))))
                                      (pipe-right (pipe-at (gamestate-grid gs) (goo-flow-row (gamestate-gf gs)) (goo-flow-col (gamestate-gf gs))))
                                      #t)
                           (goo-flow-row (gamestate-gf gs))
                           (goo-flow-col (gamestate-gf gs)))
                          (gamestate-lop gs)
                          (gamestate-sp gs)
                          (grid-goo-propagate (gamestate-gf gs)
                                              (gamestate-grid gs))
                          (gamestate-num-replaced gs)
                          (gamestate-time-left gs))]
          [else gs])
      [(mouse=? "button-up" m)
       (if (and (pipe? (pipe-at (gamestate-grid gs) (get-rc y) (get-rc x))) (pipe-filled? (pipe-at (gamestate-grid gs) (get-rc y) (get-rc x))))
           gs
           (make-gamestate (place-pipe
                            (filter not-same-pipe? (gamestate-grid gs))
                            (first (gamestate-lop gs))
                            (get-rc y)
                            (get-rc x))
                           (rest (gamestate-lop gs))
                           (gamestate-sp gs)
                           (gamestate-gf gs)
                           (if (pipe? (pipe-at (gamestate-grid gs) (get-rc y) (get-rc x)))
                               (+ 1 (gamestate-num-replaced gs))
                               (gamestate-num-replaced gs))
                           (gamestate-time-left gs)))]
      [else gs])))

;; pipe-fantasy: GameState -> GameState
(define (pipe-fantasy initial-game-state)
  (local [; draw-handler : GameState -> Image
          (define (draw-handler gs)
            ;(if (empty? (gamestate-lop gs))
                ;(turn-green (gamestate-grid gs) (gamestate-gf gs))
                (grid->image (gamestate-grid gs) 40 10 (gamestate-lop gs) gs))]
          (big-bang initial-game-state
            [stop-when goo-stop last-scene]
            [on-mouse place-pipe-on-click]
            [on-tick count-down]
            [to-draw draw-handler])))

; goo-stop : GameState -> Boolean
; stops the game if the goo can't go any further
(define (goo-stop gs)
  (and (eq? (gamestate-gf gs) (grid-goo-propagate (gamestate-gf gs)
                                                  (gamestate-grid gs)))
       (= (gamestate-time-left gs) 0)))
(check-expect (goo-stop GAME4) #f)
(check-expect (goo-stop (make-gamestate (list (make-pipe-rc PIPE-SPR 1 1)
                                              (make-pipe-rc PIPE-BL 1 2)
                                              (make-pipe-rc PIPE-TBLR 2 2)
                                              (make-pipe-rc PIPE-TB 3 2)
                                              (make-pipe-rc PIPE-TL 4 2)
                                              (make-pipe-rc PIPE-TR 4 1)
                                              (make-pipe-rc PIPE-TBLR 3 1)
                                              (make-pipe-rc PIPE-BR 2 1)
                                              (make-pipe-rc PIPE-LR 2 3)
                                              (make-pipe-rc PIPE-TL 2 4)
                                              6)
                                        (list PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR PIPE-TL)
                                        (make-pipe-rc PIPE-SPR 1 1)
                                        (make-goo-flow 1 1 "right")
                                        2
                                        0)) #f)
(check-expect (goo-stop (make-gamestate (list (make-pipe-rc PIPE-SPR 1 1)
                                              (make-pipe-rc (make-pipe #f #f #f #t #t) 1 1)
                                              6)
                                          (list PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR PIPE-TL)
                                          (make-pipe-rc PIPE-SPR 1 1)
                                          (make-goo-flow 1 1 "right")
                                          2
                                          0)) #t)

; last-scene : GameState -> Image
; the last image after the game stops
(define (last-scene gs)
  (grid->image (gamestate-grid (count-down gs)) 40 10 (gamestate-lop gs) (count-down gs)))

; grid-goo-propagate : GooFlow Grid -> GooFlow
; moves the goo forward by one tile.
; If the goo is stuck, produce the same goo.
(define (grid-goo-propagate gf g)
  (local [; connect-left? : pipe -> boolean
          (define (connect-left? p)
            (or (eq? p PIPE-LR)
                (eq? p PIPE-TR)
                (eq? p PIPE-BR)
                (eq? p PIPE-TBLR)
                (equal? p (make-pipe #t #t #t #t #t))))
          ; connect-right? : pipe -> boolean
          (define (connect-right? p)
            (or (eq? p PIPE-LR)
                (eq? p PIPE-TL)
                (eq? p PIPE-BL)
                (eq? p PIPE-TBLR)
                (equal? p (make-pipe #true #true #true #true #true))))
          ; connect-up? : pipe -> boolean
          (define (connect-up? p)
            (or (eq? p PIPE-TB)
                (eq? p PIPE-BL)
                (eq? p PIPE-BR)
                (eq? p PIPE-TBLR)
                (equal? p (make-pipe #t #t #t #t #t))))
          ; connect-down? : pipe -> boolean
          (define (connect-down? p)
            (or (eq? p PIPE-TB) 
                (eq? p PIPE-TL)
                (eq? p PIPE-TR)
                (eq? p PIPE-TBLR)
                (equal? p (make-pipe #t #t #t #t #t))))]
  (cond
    [(string=? (goo-flow-dir gf) "left")
     (cond
       [(and (pipe? (pipe-at g (goo-flow-row gf) (- (goo-flow-col gf) 1)))
             (connect-left? (pipe-at g (goo-flow-row gf) (- (goo-flow-col gf) 1)))) 
        (make-goo-flow (goo-flow-row gf) (- (goo-flow-col gf) 1) (cond
                                                                   [(eq? (pipe-at g (goo-flow-row gf) (- (goo-flow-col gf) 1)) PIPE-TR) "up"]
                                                                   [(eq? (pipe-at g (goo-flow-row gf) (- (goo-flow-col gf) 1)) PIPE-BR) "down"]
                                                                   [else "left"]))]                                                          
       [else gf])]
    [(string=? (goo-flow-dir gf) "right")
     (cond
       [(and (pipe? (pipe-at g (goo-flow-row gf) (+ (goo-flow-col gf) 1)))
             (connect-right? (pipe-at g (goo-flow-row gf) (+ (goo-flow-col gf) 1))))
        (make-goo-flow (goo-flow-row gf) (+ (goo-flow-col gf) 1) (cond
                                                                   [(eq? (pipe-at g (goo-flow-row gf) (+ (goo-flow-col gf) 1)) PIPE-TL) "up"]
                                                                   [(eq? (pipe-at g (goo-flow-row gf) (+ (goo-flow-col gf) 1)) PIPE-BL) "down"]
                                                                   [else "right"]))]
       [else gf])]
    [(string=? (goo-flow-dir gf) "up")
     (cond
       [(and (pipe? (pipe-at g (- (goo-flow-row gf) 1) (goo-flow-col gf)))
             (connect-up? (pipe-at g (- (goo-flow-row gf) 1) (goo-flow-col gf))))
        (make-goo-flow (- (goo-flow-row gf) 1) (goo-flow-col gf) (cond
                                                                   [(eq? (pipe-at g (- (goo-flow-row gf) 1) (goo-flow-col gf)) PIPE-BL) "left"]
                                                                   [(eq? (pipe-at g (- (goo-flow-row gf) 1) (goo-flow-col gf)) PIPE-BR) "right"]
                                                                   [else "up"]))]
       [else gf])]
    [(string=? (goo-flow-dir gf) "down")
     (cond
       [(and (pipe? (pipe-at g (+ (goo-flow-row gf) 1) (goo-flow-col gf)))
             (connect-down? (pipe-at g (+ (goo-flow-row gf) 1) (goo-flow-col gf))))
        (make-goo-flow (+ (goo-flow-row gf) 1) (goo-flow-col gf) (cond
                                                                   [(eq? (pipe-at g (+ (goo-flow-row gf) 1) (goo-flow-col gf)) PIPE-TL) "left"]
                                                                   [(eq? (pipe-at g (+ (goo-flow-row gf) 1) (goo-flow-col gf)) PIPE-TR) "right"]
                                                                   [else "down"]))]
       [else gf])])))
(check-expect (grid-goo-propagate GOO-RIGHT GRID-1) (make-goo-flow 2 5 "right"))
(check-expect (grid-goo-propagate GOO-LEFT GRID-1) GOO-LEFT)
(check-expect (grid-goo-propagate
               (make-goo-flow 2 1 "right")
               (list
                (make-pipe-rc (make-pipe #true #true #true #true #true) 2 1)
                (make-pipe-rc (make-pipe #true #true #true #true #true) 3 1)
                (make-pipe-rc (make-pipe #true #false #false #true #true) 4 1)
                (make-pipe-rc (make-pipe #true #false #true #false #true) 4 2)
                (make-pipe-rc (make-pipe #true #true #false #false #true) 3 2)
                (make-pipe-rc (make-pipe #true #true #true #true #true) 2 2)
                (make-pipe-rc (make-pipe #false #true #true #false #true) 1 2)
                (make-pipe-rc (make-pipe #false #false #false #true #true) 1 1)
                6)) (make-goo-flow 2 2 "right"))


; gamestate-init : Number Number Number String [List-of Pipe] -> GameState
; initializes a gamestate based on
; - grid dimensions
; - r and c coordinates of starting pipe
; - direction of starting pipe
; - list of incoming pipes
(define (gamestate-init n r c d lop)
  (make-gamestate (list (make-pipe-rc (cond
                                        [(string=? "left" d) PIPE-SPL]
                                        [(string=? "right" d) PIPE-SPR]
                                        [(string=? "up" d) PIPE-SPT]
                                        [(string=? "down" d) PIPE-SPB]) r c)
                        n)
                  lop
                  (make-pipe-rc
                   (cond
                     [(string=? "left" d) PIPE-SPL]
                     [(string=? "right" d) PIPE-SPR]
                     [(string=? "up" d) PIPE-SPT]
                     [(string=? "down" d) PIPE-SPB])
                   r c)
                  (make-goo-flow r c d)
                  0
                  140))
(check-expect (gamestate-init 7 4 3 "left" ALL-PIPES) (make-gamestate (list (make-pipe-rc PIPE-SPL 4 3) 7)
                                                                      ALL-PIPES
                                                                      (make-pipe-rc PIPE-SPL 4 3)
                                                                      (make-goo-flow 4 3 "left")
                                                                      0
                                                                      140))
(check-expect (gamestate-init 9 1 1 "right" ALL-PIPES) (make-gamestate (list (make-pipe-rc PIPE-SPR 1 1) 9)
                                                                       ALL-PIPES
                                                                       (make-pipe-rc PIPE-SPR 1 1)
                                                                       (make-goo-flow 1 1 "right")
                                                                       0
                                                                       140))
(check-expect (gamestate-init 3 2 1 "down" ALL-PIPES) (make-gamestate (list (make-pipe-rc PIPE-SPB 2 1) 3)
                                                                      ALL-PIPES
                                                                      (make-pipe-rc PIPE-SPB 2 1)
                                                                      (make-goo-flow 2 1 "down")
                                                                      0
                                                                      140))


; gamestate examples
(define GAME4 (gamestate-init 10 4 3 "up" ALL-PIPES))
(define GAME5 (gamestate-init 25 20 10 "down" ALL-PIPES))

; get-score: GameState -> Integer
; calculates the current score of the game
(define (get-score gs)
  (local [; path-length : Grid -> Number
          (define (path-length g)
            (length (filter (λ (x) (and (pipe-rc? x) (pipe-filled? (pipe-rc-pipe x)))) g)))]
  (* 50 (- (path-length (gamestate-grid gs)) (gamestate-num-replaced gs)))))
(check-expect (get-score (make-gamestate
                          (list
                           (make-pipe-rc (make-pipe #true #true #true #true #true) 4 3)
                           (make-pipe-rc (make-pipe #true #true #true #true #false) 4 3)
                           10)
                          '()
                          (make-pipe-rc (make-pipe #true #false #false #false #false) 4 3)
                          (make-goo-flow 4 3 "up")
                          7
                          140)) -300)
(check-expect (get-score GAME4) 0)
(check-expect (get-score (make-gamestate (list (make-pipe-rc PIPE-SPR 1 1)
                                                (make-pipe-rc PIPE-BL 1 2)
                                                (make-pipe-rc PIPE-TBLR 2 2)
                                                (make-pipe-rc PIPE-TB 3 2)
                                                (make-pipe-rc PIPE-TL 4 2)
                                                (make-pipe-rc PIPE-TR 4 1)
                                                (make-pipe-rc PIPE-TBLR 3 1)
                                                (make-pipe-rc PIPE-BR 2 1)
                                                (make-pipe-rc PIPE-LR 2 3)
                                                (make-pipe-rc PIPE-TL 2 4)
                                                6)
                                         (list PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR PIPE-TL)
                                         (make-pipe-rc PIPE-SPR 1 1)
                                         (make-goo-flow 1 1 "right")
                                         2
                                         28)) -100)
(check-expect (get-score (make-gamestate (list (make-pipe-rc (make-pipe #t #t #t #t #t) 1 1)
                                               (make-pipe-rc (make-pipe #t #t #f #t #t) 1 2)
                                               (make-pipe-rc PIPE-TBLR 2 2)
                                               (make-pipe-rc PIPE-TB 3 2)
                                               (make-pipe-rc PIPE-TL 4 2)
                                               (make-pipe-rc PIPE-TR 4 1)
                                               (make-pipe-rc PIPE-TBLR 3 1)
                                               (make-pipe-rc PIPE-BR 2 1)
                                               (make-pipe-rc PIPE-LR 2 3)
                                               (make-pipe-rc PIPE-TL 2 4)
                                               6)
                                         (list PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR PIPE-TL)
                                         (make-pipe-rc PIPE-SPR 1 1)
                                         (make-goo-flow 1 1 "right")
                                         1
                                         28)) 50)


; count-down : GameState -> GameState
; counts down the timer before the goo propogates
(define (count-down gs)
  (local [; not-same-pipe? : Pipe-RCOrNumber -> Boolean
          (define (not-same-pipe? pon)
            (cond
              [(number? pon) #t]
              [(pipe-rc? pon)
               (cond
                 [(equal? (make-pipe-rc (make-pipe (pipe-top (pipe-at (gamestate-grid gs) (goo-flow-row (gamestate-gf gs)) (goo-flow-col (gamestate-gf gs)))) 
                                                   (pipe-bot (pipe-at (gamestate-grid gs) (goo-flow-row (gamestate-gf gs)) (goo-flow-col (gamestate-gf gs))))
                                                   (pipe-left (pipe-at (gamestate-grid gs) (goo-flow-row (gamestate-gf gs)) (goo-flow-col (gamestate-gf gs))))
                                                   (pipe-right (pipe-at (gamestate-grid gs) (goo-flow-row (gamestate-gf gs)) (goo-flow-col (gamestate-gf gs))))
                                                   #t)
                                        (goo-flow-row (gamestate-gf gs))
                                        (goo-flow-col (gamestate-gf gs))) pon)
                  #f]
                 [else #t])]))]
    (cond
      [(> (gamestate-time-left gs) 0)
       (make-gamestate (gamestate-grid gs)
                       (gamestate-lop gs)
                       (gamestate-sp gs)
                       (gamestate-gf gs)
                       (gamestate-num-replaced gs)
                       (- (gamestate-time-left gs) 1))]
      [else
       (make-gamestate (place-pipe
                        (filter not-same-pipe? (gamestate-grid gs))
                        (make-pipe (pipe-top (pipe-at (gamestate-grid gs) (goo-flow-row (gamestate-gf gs)) (goo-flow-col (gamestate-gf gs)))) 
                                   (pipe-bot (pipe-at (gamestate-grid gs) (goo-flow-row (gamestate-gf gs)) (goo-flow-col (gamestate-gf gs))))
                                   (pipe-left (pipe-at (gamestate-grid gs) (goo-flow-row (gamestate-gf gs)) (goo-flow-col (gamestate-gf gs))))
                                   (pipe-right (pipe-at (gamestate-grid gs) (goo-flow-row (gamestate-gf gs)) (goo-flow-col (gamestate-gf gs))))
                                   #t)
                        (goo-flow-row (gamestate-gf gs))
                        (goo-flow-col (gamestate-gf gs)))
                       (gamestate-lop gs)
                       (gamestate-sp gs)
                       (grid-goo-propagate (gamestate-gf gs)
                                           (gamestate-grid gs))
                       (gamestate-num-replaced gs)
                       28)])))
  (check-expect (count-down (make-gamestate (list (make-pipe-rc PIPE-SPR 1 1)
                                                  6)
                                            (list PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR PIPE-TL)
                                            (make-pipe-rc PIPE-SPR 1 1)
                                            (make-goo-flow 1 1 "right")
                                            2
                                            28))
                (make-gamestate
                 (list (make-pipe-rc PIPE-SPR 1 1)
                       6)
                 (list PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR PIPE-TL)
                 (make-pipe-rc PIPE-SPR 1 1)
               (make-goo-flow 1 1 "right")
               2
               27))
(check-expect (count-down (make-gamestate (list (make-pipe-rc PIPE-SPR 1 1)
                                                6)
                                          (list PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR PIPE-TL)
                                          (make-pipe-rc PIPE-SPR 1 1)
                                          (make-goo-flow 1 1 "right")
                                          2
                                          0))
              (make-gamestate
               (list (make-pipe-rc (make-pipe #f #f #f #t #t) 1 1)
                     (make-pipe-rc PIPE-SPR 1 1)
                     6)
               (list PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR PIPE-TL)
               (make-pipe-rc PIPE-SPR 1 1)
               (make-goo-flow 1 1 "right")
               2
               28))
(check-expect (count-down (make-gamestate (list (make-pipe-rc PIPE-SPR 1 1)
                                                6)
                                          (list PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR PIPE-TL)
                                          (make-pipe-rc PIPE-SPR 1 1)
                                          (make-goo-flow 1 1 "right")
                                          2
                                          14))
              (make-gamestate
               (list (make-pipe-rc PIPE-SPR 1 1)
                     6)
               (list PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR PIPE-TL)
               (make-pipe-rc PIPE-SPR 1 1)
               (make-goo-flow 1 1 "right")
               2
               13))


(define GAME-DEMO (make-gamestate (list (make-pipe-rc PIPE-SPR 1 1)
                                        (make-pipe-rc PIPE-BL 1 2)
                                        (make-pipe-rc PIPE-TBLR 2 2)
                                        (make-pipe-rc PIPE-TB 3 2)
                                        (make-pipe-rc PIPE-TL 4 2)
                                        (make-pipe-rc PIPE-TR 4 1)
                                        (make-pipe-rc PIPE-TBLR 3 1)
                                        (make-pipe-rc PIPE-BR 2 1)
                                        (make-pipe-rc PIPE-LR 2 3)
                                        (make-pipe-rc PIPE-TL 2 4)
                                        6)
                                  (list PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR PIPE-TL)
                                  (make-pipe-rc PIPE-SPR 1 1)
                                  (make-goo-flow 1 1 "right")
                                  0
                                  28))

; write a function which filters out same items in count down, as we dont want to double count a tblr pipe
                                                       





 