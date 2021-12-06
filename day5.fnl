(local fennel (require :fennel))
(local utils (require :utils))

(fn parse-line [ line ]
  (let [(_ _ x1 y1 x2 y2) (string.find line "(%d+),(%d+)%s*->%s*(%d+),(%d+)")]
    {:x1 (tonumber x1) :y1 (tonumber y1) :x2 (tonumber x2) :y2 (tonumber y2)}))

(fn parse-inp [inp] 
  "Returns a table of tables with keys x1,y1,x2,y2"
  (icollect [line (io.lines inp)] (parse-line line)))

(fn filter-aligned [intab]
  "takes a table and returns a new one with just axis-aligned lines"
  (icollect [_ v (ipairs intab)]
      (when (or (= v.x1 v.x2) (= v.y1 v.y2)) v)))

(fn is-diag? [{: x1 : y1 : x2 : y2}]
  "Diagonal lines will have the same difference between x and y"
  (= (math.abs (- x1 x2)) (math.abs (- y1 y2))))
;(is-diag? {:x1 0 :y1 5 :x2 5 :y2 0})
(fn filter-diag [lines]
  (icollect [_ v (ipairs lines)] (when (is-diag? v) v)))

(fn get-points [vtxs]
  "Returns a table with explicit points from the two ends if the line is aligned"
  (var l {})
  (for [x (math.min vtxs.x1 vtxs.x2) (math.max vtxs.x1 vtxs.x2)]
    (for [y (math.min vtxs.y1 vtxs.y2) (math.max vtxs.y1 vtxs.y2)]
      (table.insert l {:x x :y y})))
  l)

(fn get-points-diag [vtxs]
  "Returns a table with explicit points between the two ends"
  (var l {})
  (local stepx (if (> vtxs.x1 vtxs.x2) -1 (= vtxs.x1 vtxs.x2) 0 1))
  (local stepy (if (> vtxs.y1 vtxs.y2) -1 (= vtxs.y1 vtxs.y2) 0 1))
  (var y vtxs.y1)
  (for [x vtxs.x1 vtxs.x2 stepx]
    (table.insert l {:x x :y y})
    (set y (+ y stepy)))
  l)

(fn tab-inc [tbl k] 
  "Increment value at k (or insert 1)"
  (tset tbl k (+ 1 (or (. tbl k) 0))))

; Need this because otherwise tables as keys is not great...
(fn str [{: x : y}] (.. "x:" x "y:" y))

(fn fill-lines [lines] 
  "for each line, push all the points into the accumulator with counts"
  (local acct {})
  (each [_ line (ipairs lines)]
    (each [_ p (ipairs (get-points line))]
      (tab-inc acct (str p))))
  acct)

(fn fill-diag-lines [acc lines] 
  "add diagonal lines to the points acc"
  (each [_ line (ipairs lines)]
    (each [_ p (ipairs (get-points-diag line))]
      (tab-inc acc (str p))))
  acc)

(fn count-2-plus [pts]
  (var acc 0)
  (each [xy v (pairs pts)] (when (> v 1) (set acc (+ 1 acc)))); 
  acc)

;;;;;;;;;;;;;;;;;
(local inp "inp/day5.txt")
(local lines (parse-inp inp))
(local pmap (fill-lines (filter-aligned lines)))
(print (.. "Day5: Part1: " (utils.bold (count-2-plus pmap)) " aligned overlaps"))
(fill-diag-lines pmap (filter-diag lines))
(print (.. "Day5: Part2: " (utils.bold (count-2-plus pmap)) " aligned/diagonal overlaps"))
