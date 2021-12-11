(local fennel (require :fennel))
(local utils (require :utils))

(fn key [x  y] (.. x "," y))
(fn parse-inp [filename]
  "Returns a table where keys are strings 'x:y'"
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
  tbl)

(fn neighbors [k]
  (local (_ _ x y) (string.find k "(%d+),(%d+)"))
  (var res {})
  (for [i (- x 1) (+ x 1)]
    (for [j (- y 1) (+ y 1)]
      (when (and (> i 0) (< i 11) (> j 0) (< j 11))
        (table.insert res (key i j)))))
  res)
(fn inc-all [tbl]
  "Increments all values in a table - modifies the tabl in place + returns a table of keys > 9"
  (var res {})
  (each [k v (pairs tbl)]
    (do
      (tset tbl k (+ 1 v))
      (when (= v 9) (table.insert res k))))
  res)

(fn reset [tbl]
  "Sets all values > 9 back to 0"
  (each [k v (pairs tbl)]
    (when (> v 9) (tset tbl k 0))))


(fn print-tbl [tbl]
  "prints the table nicely with the formatting"
  (for [i 1 10]
    (var row {})
    (for [j 1 10]
      (let [v (. tbl (key j i))
            vp (if (= 0 v) (utils.bold v) v)]
                 (table.insert row vp)))
    (print (table.concat row ""))))

(fn inc-list [tbl elems]
  "increment every key in elems. returns a table of flashing keys"
  (var res {})
  (each [_ k (ipairs elems)]
    (do 
      (local v (. tbl k))
      (tset tbl k (+ 1 v))
      (when (= v 9) (table.insert res k))))
  res)

(fn step [tbl]
  "Perform 1 step of the algorithm"
  (var stack (inc-all tbl))
  (var flashes 0)
  (while (> (length stack) 0)
    (do
      (set flashes (+ 1 flashes))
      (let [k (table.remove stack)
            nbrs (neighbors k)
            new-flashes (inc-list tbl nbrs)]
        (utils.table-append stack new-flashes))))
  (reset tbl)
  flashes)

(fn step-n [tbl n]
  (var tot 0)
  (for [i 1 n]
    (set tot (+ tot (step tbl))))
  tot)

(fn step-until-sync [tbl n]
  "n is the starting value"
  (var step-count (+ 1 n))
  (while (not= (step tbl) 100)
    (set step-count (+ 1 step-count)))
  step-count)

;;;;;;;;;;;;;;;;;;;;;;;
(local inp-file "inp/day11.txt")
;(local inp-file "inp/day11_test.txt")
(var inp (parse-inp inp-file))

(comment 
(print "Before any steps:")
(print-tbl inp)
(for [i 1 10]
  (print "")
  (local stepflashes (step inp))
  (print (.. "After step " i ":"))
  (print-tbl inp)))

(local pt1 (step-n inp 100))
(print (.. "Day11: Part1: Flashes after 100 steps: " (utils.bold pt1)))

(local pt2 (step-until-sync inp 100))
(print (.. "Day11: Part2: steps until syncd: " (utils.bold pt2)))
