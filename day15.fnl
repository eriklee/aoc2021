(local fennel (require :fennel))
(local utils (require :utils))
(local heap (require :heap))

(fn key [x  y] (.. x "," y))
(fn unkey [pt]
  (let [(_ _ x y) (string.find pt "^(%d+),(%d+)$")]
    (values (tonumber x) (tonumber y))))

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

(fn neighbors [pt]
  (let [(x y) (unkey pt)]
    (var res [])
    (table.insert res (key (+ x 1) y))
    (table.insert res (key (- x 1) y))
    (table.insert res (key x (+ y 1)))
    (table.insert res (key x (- y 1)))
    res))
(neighbors "1,1")
(neighbors "3,3")

(fn init-search [inp-grid]
  "create a table with :visited :best-in :current-cost for each cell"
  (var res {:grid inp-grid})
  (var visited {})
  (var best-in {})
  (var current-cost {})
  (each [k _ (pairs inp-grid)]
            (do (tset visited k false) (tset best-in k nil) (tset current-cost k 1e9)))
  (tset visited "1,1" true)
  (tset best-in "1,1" "start")
  (tset current-cost "1,1" 0)
  (tset res :visited visited)
  (tset res :best-in best-in)
  (tset res :current-cost current-cost)
  (tset res :current-node "1,1")
  (tset res :costs (heap:new))
  res)

(fn unvisited? [pt inp] (not (. inp.visited pt)))
(fn valid? [pt inp] (not= nil (. inp.grid pt)))

(fn update-neighbors [pt input]
  "Update all unvisited neighbors from the current node"
  (local nbrs (neighbors pt))
  ;(print "visiting " pt " neighbors=" (fennel.view nbrs))
  (each [_ n (ipairs nbrs)]
    (when (and (valid? n input) ; valid node
               (unvisited? n input)) ;unvisited
      (do 
        ;(print "checking valid, unvisited nbr " n)
        (let [len (+ (. input.current-cost pt) (. input.grid n))]
            (if (< len (. input.current-cost n))
              (do (tset input.current-cost n len)
                (input.costs:push n len)
                (tset input.best-in n pt)))))))
  ; mark the current node as visited
  ;(print "finished visiting " pt)
  (tset input.visited pt true))

; 9m 3s
(fn min-unvisited [input]
  "Find a pt with the minimum distance we haven't visited"
  (var res nil)
  (var min-dist 1e19)
  (each [p d (pairs input.current-cost)]
    (when (and (unvisited? p input)
               (< d min-dist))
     (do (set res p) (set min-dist d)))) 
  res)

(fn min-unvisited-heap [input]
  (var res nil)
  (while (and (= res nil) (not (input.costs:isempty?)))
    (let [[pt _] (input.costs:pop)]
    (when (unvisited? pt input) (set res pt))))
  res)
  

(fn find-best-route [inp]
  (local goal (key inp.grid.max-x inp.grid.max-y))
  (while (unvisited? goal inp)
    (do
      (update-neighbors inp.current-node inp)
      (tset inp :current-node (min-unvisited-heap inp)))))

(fn mod-inc [n x]
  (let [res (+ n x)]
    (if (< res 10) res (- res 9))))
(mod-inc 6 5)

(fn x5-input [inp]
  (var res {})
  (let [orig-x inp.grid.max-x
        orig-y inp.grid.max-y]
    (for [i 0 4]
      (for [j 0 4]
        (local inc-v (+ i j))
        (for [x 1 orig-x]
          (for [y 1 orig-y]
            (tset res (key (+ x (* i orig-x)) (+ y (* j orig-y))) (mod-inc (. inp.grid (key x y)) inc-v))))))
    (tset res :max-x (* 5 orig-x))
    (tset res :max-y (* 5 orig-y)))
  res)

;;;;; Debug helpers
(fn routing-table [inp]
  (var next (key inp.grid.max-x inp.grid.max-y))
  (var res {})
  (while (not= :start next)
    (tset res next true)
    (set next (. inp.best-in next)))
  res)

(fn print-route [inp]
  "prints the grid nicely with the formatting"
  (local route (routing-table inp))
  (for [i 1 inp.grid.max-y]
    (var row {})
    (for [j 1 inp.grid.max-x]
      (let [k (key j i)
            v (. inp.grid k)
            vp (if (. route k) (utils.boldred v) v)]
                 (table.insert row vp)))
    (print (table.concat row ""))))

;;;;;;;;;;;;;;;;;;;;;;;
(local inp-file "inp/day15.txt")
;(local inp-file "inp/day15_s_test.txt")
;(local inp-file "inp/day15_test.txt")
(var inp (init-search (parse-inp inp-file)))
(find-best-route inp)
(local goal (key inp.grid.max-x inp.grid.max-y))
(local part1 (. inp.current-cost goal))
;(print-route inp)
(print (.. "Day15: Part1: lowest cost path is: " (utils.bold part1)))

(local big-inp (init-search (x5-input inp)))
(find-best-route big-inp)
(local goal (key big-inp.grid.max-x big-inp.grid.max-y))
(local part2 (. big-inp.current-cost goal))
;(print-route big-inp)
(print (.. "Day15: Part2: lowest cost path is: " (utils.bold part2)))
