(local fennel (require :fennel))
(local utils (require :utils))

(fn key [x  y] (.. x "," y))
(fn parse-inp [filename]
  "Returns a table where keys are strings 'x:y' and max-x and max-y give dims"
  (var tbl {})
  (var y 0)
  (var x 0)
  (each [ line (io.lines filename)]
    (do
      (set x 1)
      (set y (+ 1 y))
      (each [ w (string.gmatch line "%d")]
        (do 
          (tset tbl (key x y) (tonumber w))
          (set x (+ 1 x))))))
  (tset tbl :max-x (- x 1))
  (tset tbl :max-y y)
  tbl)

(fn lookup [tbl x y]
  "Returns the value in the table at that location, or 10"
  (or (. tbl (key x y)) 10))

(fn find-low-points [inp]
  "return a table of low point values"
  (var res {})
  (for [y 1 inp.max-y]
    (for [x 1 inp.max-x]
      (do
        (local val (lookup inp x y))
        (when (and 
            (< val (lookup inp (- x 1) y))
            (< val (lookup inp (+ x 1) y))
            (< val (lookup inp x (- y 1)))
            (< val (lookup inp x (+ y 1))))
          (table.insert res (key x y))))))
  res)

(fn sum-risk-levels [inp lps]
  "Given a table a low point values, return the sum of the risk levels"
  (accumulate [tot 0
               _ lp (ipairs lps)]
              (+ tot 1 (. inp lp))))

(fn continue? [inp res x y]
  "should we search this square"
  (and (= nil (. res (key x y))) ; we haven't seen it already
       (< (lookup inp x y) 9))) ; it isn't an edge or a 9

(fn fill-basins [inp lps]
  "return a table with same keys as inputs and values are a basin id"
  (var res {})
  (var basinid 1)
  (each [_ lp (ipairs lps)]
    (var search-tbl [lp])
    (while (> (length search-tbl) 0)
      (do ; for each low point, search in all directions for non-9, non-edge values
      (local pt (table.remove search-tbl))
      (local (_ _ x y) (string.find pt "(%d+),(%d+)"))
      (when (continue? inp res (- x 1) y) (table.insert search-tbl (key (- x 1) y)))
      (when (continue? inp res (+ x 1) y) (table.insert search-tbl (key (+ x 1) y)))
      (when (continue? inp res x (- y 1)) (table.insert search-tbl (key x (- y 1))))
      (when (continue? inp res x (+ y 1)) (table.insert search-tbl (key x (+ y 1))))
      (tset res pt basinid)))
    (set basinid (+ 1 basinid)))
  res)

(fn get-basin-sz [basin-map]
  "Returns a map of basin-id -> size"
  (var res {})
  (each [l bid (pairs basin-map)]
    (tset res bid (+ 1 (or (. res bid) 0))))
  res)

(fn prod-top-3 [szs]
  (table.sort szs)
  (var res 1)
  (for [i 1 3]
      (set res (* res (table.remove szs))))
  res)

;;;;;;;;;;;;;;;;;;;;;;;
(local inp-file "inp/day9.txt")
;(local inp-file "inp/day9_test.txt")
(var inp (parse-inp inp-file))

(local lps (find-low-points inp))
(local part1 (sum-risk-levels inp lps))
(print (.. "Day9: Part1: Sum of risk levels is: " (utils.bold part1)))

(local filled (fill-basins inp lps))
(local szs (get-basin-sz filled))
(local part2 (prod-top-3 szs)) ; 1134 too low
(print (.. "Day9: Part2: Sum of all decoded outputs: " (utils.bold part2)))
