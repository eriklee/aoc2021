(local fennel (require :fennel))
(local utils (require :utils))

(fn parse-inp [filename]
  "returns a table where each entry is the number of fish there.
   Note the life-values are 1 higher than the puzzle"
  (var tbl {})
  (with-open [fin (io.open filename)]
             (each [ w (string.gmatch (fin:read) "%d")]
               (utils.tab-inc tbl (+ 1 (tonumber w))))
             tbl)
  (for [i 1 9] (tset tbl i (or (. tbl i) 0)))
  tbl
  )

(fn step-state [tbl]
  "Iterates 1 life-cycle of the lanternfish. such beauty!"
  (var ns {})
  (for [i 1 9]
    (local x (. tbl i))
    (if (= i 1)
      (do
        (tset ns 7 x)
        (tset ns 9 x))
      (tset ns
            (- i 1)
            (+ x (or (. ns (- i 1)) 0))
        )
      )
    )
  ns)

;;;;;;;;
(local inp "inp/day6.txt")

(var start (parse-inp inp))
(for [i 1 80] (set start (step-state start)))
(print (.. "Day6: Part1: after 80 days there are " (utils.bold (utils.sum-table start)) " lanternfish!"))

(for [i 81 256] (set start (step-state start)))
(print (.. "Day6: Part2: after 256 days there are " (utils.bold (utils.sum-table start)) " lanternfish!"))
