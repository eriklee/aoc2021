(local fennel (require :fennel))
(local utils (require :utils))
(fn pp [x] (print (fennel.view x)))

(fn clone [tbl] (var t2 []) (each [_ v (ipairs  tbl)] (table.insert t2 v)) t2)

(fn append-val [tbl k nv]
  "Appends nv to the list value of tbl k or inserts it"
  (tset tbl k (or (. tbl k) {}))
  (table.insert (. tbl k) nv))

;(var tbl {:k [1]})
;(append-val tbl :k 2)
;(append-val tbl :y 1)
;(print (fennel.view tbl))

(fn parse-inp [inp]
  "return a table mapping every node to every neighbor"
  (var edges {})
  (each [l (io.lines inp)]
    (let [(_ _ a b) (string.find l "([^-]*)-(.*)")] 
      (append-val edges a b)
      (append-val edges b a)))
  edges)

(fn pop-back [progress]
  ;(print "pop-back: " (fennel.view progress))
  (local last (table.remove progress))
  (if (= 0 (length last))
    nil ; no more edges from the current node
    (let [ll (table.remove last)] (table.insert progress last) ll)))

;(var tbl [ [ 1 2 3] [ 1 2 ] ])
;(pop-back tbl)
;(print (fennel.view tbl))
;(pop-back tbl)
;(print (fennel.view tbl))
;(pop-back tbl)
;(print (fennel.view tbl))
;(pop-back tbl)
;(print (fennel.view tbl))
;(pop-back tbl)
;(print (fennel.view tbl))

(fn contains [curr-path next]
  (accumulate [res false
               _ p (ipairs curr-path)]
              (or res (= p next))))

(fn can-visit1 [curr-path next]
  (or (= (string.upper next) next) ; if next is upper-cased it's fine
      (not (contains curr-path next)))) ; or it's not in the path

(fn is-lower [x] (= (string.lower x) x))
(fn has-dup-lower [path]
  (local p2 [])
  (each [_ x (ipairs path)] (when (is-lower x) (table.insert p2 x)))
  (table.sort p2)
  (var res false)
  (for [i 1 (- (length p2) 1)]
    (set res (or res (= (. p2 i) (. p2 (+ i 1))))))
  res)

(has-dup-lower [:a :b :A])
(has-dup-lower [:a :b :a])
(fn is-upper [x] (= x (string.upper x)))
(is-upper :a)
(is-upper :A)

(fn can-visit2 [curr-path next]
  ;(print "can visit: " (fennel.view curr-path) next)
  (and (not= :start next)
       (or (is-upper next) ; an upper case one is always good
           (or (not (has-dup-lower curr-path)) ; if there's not a dup yet it's fine
               (not (contains curr-path next))))) ; or it's not in the path
)

(contains [1 2 3] 1)
(contains [1 2 3] 4)
(can-visit1 [:a :B :C] :a)
(can-visit1 [:a :B :C] :b)
(can-visit1 [:a :B :C] :B)
(can-visit2 [:a :b :C :b] :a)
(can-visit2 [:a :B :C] :b)
(can-visit2 [:a :B :C] :a)
(can-visit2 [:start :b :A :c :A :b :end] :A)
(can-visit2 [:start :b :A :c  ] :A)
(can-visit2 [:start :b :A :c :A] :b)

(fn search [edges can-visit]
  "DFS through the edges following the rules"
  (var full-paths {})
  (var curr-path [ :start ])
  (var rem-edges [ (clone edges.start) ])
  (while (not= (length curr-path) 0)
    ; look at the back element and look to see if we can go into it
    (let [next (pop-back rem-edges)]
      (if
        (= next nil) (table.remove curr-path) ; finished this, reverse!
        (= next :end) (do ; found a valid path, record it!
                        ;(print (fennel.view curr-path))
                        (table.insert curr-path :end)
                        ;(print "doing" (table.concat curr-path ","))
                        (table.insert full-paths (table.concat curr-path ","))
                        (table.remove curr-path)) 
        (can-visit curr-path next) (do ; another extension, explore
                                     ;(print "on " (fennel.view curr-path) " entering " next)
                                     ;(print "rem-edges: " (fennel.view rem-edges))
                                     (table.insert curr-path next)
                                     (table.insert rem-edges (clone (. edges next))))
        nil)) ; oh well, we tried
    )
  full-paths)

;;;;;;;;;;;;;;;;;;;;;;;
(local inp-file "inp/day12.txt")
;(local inp-file "inp/day12_test.txt")
(var edges (parse-inp inp-file))
;(pp edges)
(local paths (search edges can-visit1))
(print "Day12: Part1: There are " (utils.bold (length paths)) " unique paths")
(local paths (search edges can-visit2))
;(pp paths)
(print "Day12: Part2: There are " (utils.bold (length paths)) " unique paths (with slightly different rules...)")
