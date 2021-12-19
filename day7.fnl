(local fennel (require :fennel))
(local utils (require :utils))
(local unpack (or table.unpack _G.unpack))

(fn parse-inp [filename]
  "returns a table where each entry is a crab position"
  (var tbl {})
  (with-open [fin (io.open filename)]
             (each [ w (string.gmatch (fin:read) "%d+")]
               (table.insert tbl (tonumber w)))
             tbl)
  )

(fn fuel-cost-linear [inp pos]
  "finds the fuel cost of moving the inp crabs to pos"
  ;(print :linear)
  (accumulate [tot 0
               _ x (ipairs inp)]
    (+ tot (math.abs (- x pos)))))

(fn tri [x] (/ (* x (+ x 1)) 2))

(fn fuel-cost-exp [inp pos]
  "finds the fuel cost of moving the inp crabs to pos"
  ;(print :exp)
  (accumulate [tot 0
               _ x (ipairs inp)]
    (+ tot (tri (math.abs (- x pos))))))

(fn minimal-fuel [f inp]
  "returns the position and fuel cost of the cheapest meeting place given the cost function"
  (var minfuel (/ 1 0)) ; inf
  (for [i 0 (math.max (unpack inp))]
    (set minfuel (math.min minfuel (f inp i)))
  )
  minfuel)

(fn mfs-w [f inp g]
  "given a starting loc move towards the minimal location"
  (local cur (f inp g))
  (local p1 (f inp (+ 1 g)))
  (if (> cur p1) ; we need to move up
    (mfs-w f inp (+ g 1))
    (> cur (f inp (- g 1))) ; we need to move down
    (mfs-w f inp (- g 1))
    cur) ; found the min!
  )

(fn minimal-fuel-search [f inp]
  (local avg (math.floor (/ (utils.sum-table inp) (length inp))))
  (mfs-w f inp avg))

;;;;;;;;
(local inp "inp/day7.txt")
;(local inp "inp/day7_test.txt")

(var start (parse-inp inp))

;(local minl (minimal-fuel fuel-cost-linear start))
(local minls (minimal-fuel-search fuel-cost-linear start))
(print (.. "Day7: Part1: min fuel req (linear) " (utils.bold minls)))

;(local mine (minimal-fuel fuel-cost-exp start))
(local mines (minimal-fuel-search fuel-cost-exp start))
(print (.. "Day7: Part2: min fuel req (tri) " (utils.bold mines)))
