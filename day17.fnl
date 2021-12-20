(local fennel (require :fennel))
(local utils (require :utils))

(fn parse-inp [inp-file]
  (with-open [f (io.open inp-file)]
             (local line ((f:lines)))
             (let [(_ _ xmin xmax ymin ymax) (string.find line "^target area: x=(%d+)%.%.(%d+), y=(%-%d+)..(%-%d+)$")]
               {:xmin (tonumber xmin) :xmax (tonumber xmax)
                :ymin (tonumber ymin) :ymax (tonumber ymax)})))

(fn init-probe [xv yv]
  {:x 0 :y 0 :xvel xv :yvel yv :stepcount 0 :max-y 0})

(fn step-probe [p]
  (tset p :stepcount (+ 1 p.stepcount))
  (tset p :x (+ p.x p.xvel))
  (tset p :y (+ p.y p.yvel))
  (tset p :max-y (math.max p.y p.max-y))
  (tset p :xvel (math.max 0 (- p.xvel 1))) ; I don't think this would ever be negative to begin with?
  (tset p :yvel (- p.yvel 1)))

(fn missed? [p ta]
  (or (> p.x ta.xmax) ; already too far over
      (< p.y ta.ymin)) ; already too far down
  )

(fn hit-target? [p ta]
  (and (>= p.x ta.xmin)
       (<= p.x ta.xmax)
       (>= p.y ta.ymin)
       (<= p.y ta.ymax)))

(fn test-trajectory [xv yv ta]
  "checks whether a probe shot with the xv,yv will hit the ta"
  (var p (init-probe xv yv))
  (while (and (not (missed? p ta)) (not (hit-target? p ta)))
    (step-probe p))
  (if (hit-target? p ta)
    p
    nil))
   ;(do (print "hit! " xv "," yv "=> p" (fennel.view p)) p)
   ;(do (print "miss " xv "," yv "=> p" (fennel.view p)) nil)))

(fn ppp [p ta] 
  (print :x p.x :y p.y :xv p.xvel :yv p.yvel :max-y p.max-y :hit? (hit-target? p ta) :missed? (missed? p ta)))

; it seems like it would be good to get your final x speed to be 0 if possible, so
; it needs to be inv-tri xmin <= xvel <= inv-tri xmax (any of those is the same)
; y is parabolic so we want to figure out a y which would end up in the right range stepping down from 0
; I think that means it is |y-min + 1|?
; lets just check the boundaries of those ranges to be safe...

; (fn tri [x] (/ (* x (+ x 1)) 2)) ; say hello to an old fan favorite
(fn inv-tri [x] (math.floor (math.sqrt (* 2 x))))

(fn find-highest-y [ta]
  (local xv (inv-tri ta.xmin))
  (var best-p {:max-y 0}) ; fake it till you make it
  (for [yv (math.abs ta.ymax) (math.abs ta.ymin)]
    (local p (test-trajectory xv yv ta))
    (when (and (not= nil p) (>= p.max-y best-p.max-y))
      (set best-p p)
      (tset p :init-y yv)
      (tset p :init-x xv)))
  best-p)

; min xv is (inv-tri ta.xmin)
; max yv is the highest from before
; can keep increasing x until you get to xmin - 1
; y drops off pretty fast. just start from ymax and try it all until the first max
; it's not actually that many...

; then there's the square where you yeet them all in in one step
(fn box-size [ta]
  (* (+ 1 (- ta.xmax ta.xmin))
     (+ 1 (- ta.ymax ta.ymin))))
(assert (= 66 (box-size (parse-inp "inp/day17_test.txt"))) "box size calc is wrong")

(fn find-all-valids [ta]
  (local hp (find-highest-y ta))
  (local min-xv hp.init-x)
  (local max-xv (- ta.xmin 1))
  (local min-yv (+ 1 ta.ymax))
  (local max-yv hp.init-y)
  (var hits (box-size ta))
  (for [xv min-xv max-xv]
    (for [yv min-yv max-yv]
      (match (test-trajectory xv yv ta)
        nil 1
        p (set hits (+ 1 hits)))))
  hits)


;;;;;;;;;;;;;;;
(local inp-file "inp/day17.txt")
;(local inp-file "inp/day17_test.txt")
(local target-area (parse-inp inp-file))

;(local t1 (test-trajectory 7 2 target-area))
;(assert (and (not= nil t1) (= t1.max-y 3)) "test inp1")
;(local t1 (test-trajectory 6 3 target-area))
;(assert (and (not= nil t1) (= t1.max-y 6)) "test inp2")
;(local t1 (test-trajectory 7 10 target-area))
;(assert (and (not= nil t1) (= t1.max-y 45)) "test inp2")

(local part1p (find-highest-y target-area))
;(ppp part1p target-area)
(local part1 part1p.max-y)
(print (.. "Day17: Part1: highest point possible is: " (utils.bold part1)))
(local part2 (find-all-valids target-area))
(print (.. "Day17: Part2: result of evaluating the pkt is: " (utils.bold part2)))

;;; a slightly more nicely organized version of the table from the example from part 2
;                                                               6,0 6,1 6,2 6,3 6,4 6,5 6,6 6,7 6,8 6,9
;                                                          7,-1 7,0 7,1 7,2 7,3 7,4 7,5 7,6 7,7 7,8 7,9
;                                                    8,-2  8,-1 8,0 8,1
;                                                    9,-2  9,-1 9,0
;                                                   10,-2 10,-1
;                                       11,-4 11,-3 11,-2 11,-1
;                                       12,-4 12,-3 12,-2
;                                       13,-4 13,-3 13,-2
;                                       14,-4 14,-3 14,-2
;                                       15,-4 15,-3 15,-2
; 20,-10 20,-9 20,-8 20,-7 20,-6 20,-5
; 21,-10 21,-5 21,-6 21,-7 21,-8 21,-9
; 22,-10 22,-5 22,-6 22,-7 22,-8 22,-9
; 23,-10 23,-5 23,-6 23,-7 23,-8 23,-9
; 24,-10 24,-5 24,-6 24,-7 24,-8 24,-9
; 25,-10 25,-5 25,-6 25,-7 25,-8 25,-9
; 26,-10 26,-5 26,-6 26,-7 26,-8 26,-9
; 27,-10 27,-5 27,-6 27,-7 27,-8 27,-9
; 28,-10 28,-5 28,-6 28,-7 28,-8 28,-9
; 29,-10 29,-5 29,-6 29,-7 29,-8 29,-9
; 30,-10 30,-5 30,-6 30,-7 30,-8 30,-9
