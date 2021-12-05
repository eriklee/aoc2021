(local fennel (require :fennel))
(global pp (fn [x] (print (fennel.view x))))

(fn parse-inp [inp] 
  "Tries to parse inp, returning a table with:
  - nums (the numbers in order)
  - [1..n] the boards flattened and converted to numbers"
  (let [f (assert (io.open inp))
        inputs {}]
    (set inputs.nums
         (icollect [num (string.gmatch (f:read) "%d+")] (tonumber num)))
    (while (not (= nil (f:read)))
      (let [t {}]
        (for [i 1 5] (table.insert t i (f:read)))
        (table.insert inputs 
                      (icollect [num (string.gmatch (table.concat t " ") "%d+")] (tonumber num)))
        ))
    (f:close)
    inputs))

; true = called, so a number has not yet been done
(fn test-idx [tab idx] (= "boolean" (type (. tab idx))))

(fn is-winning-row [tab idx]
  (let [rowstart (+ 1 (* (- idx 1) 5))] ; lol 1 based indexing :(
    (and (test-idx tab rowstart)
         (test-idx tab (+ 1 rowstart))
         (test-idx tab (+ 2 rowstart))
         (test-idx tab (+ 3 rowstart))
         (test-idx tab (+ 4 rowstart)))))

(fn is-winning-col [tab idx]
  (and (test-idx tab idx)
       (test-idx tab (+ idx 5))
       (test-idx tab (+ idx 10))
       (test-idx tab (+ idx 15))
       (test-idx tab (+ idx 20))))

(fn is-winner? [tab]
  "Determines whether a table has won yet"
  (or (is-winning-row tab 1)
      (is-winning-row tab 2)
      (is-winning-row tab 3)
      (is-winning-row tab 4)
      (is-winning-row tab 5)
      (is-winning-col tab 1)
      (is-winning-col tab 2)
      (is-winning-col tab 3)
      (is-winning-col tab 4)
      (is-winning-col tab 5)))

(fn sum-table [tab]
  "Returns the sum of all numeric values in the table"
  (var sum 0)
  (each [i x (ipairs tab)]
    (when (= "number" (type x)) (set sum (+ sum x))))
  sum)

; Returns a score for the board (regardless of winning status)
(fn score-board [tab final-number] (* final-number (sum-table tab)))

(fn call-number [number board]
  "If the number appears on the board sets it to true"
  (each [i v (ipairs board)]
    (when (= v number) (tset board i true))))

(fn iterate-board [board numbers]
  "applies numbers to board until the board wins
  then returns (index of final number, score)"
  (var nidx 0)
  (while (not (is-winner? board)) :until (> nidx 100)
    (set nidx (+ nidx 1))
    (call-number (. numbers nidx) board))
  (values nidx (score-board board (. numbers nidx))))

(fn find-winner [ inputs ]
  "Iterates over all boards and returns the idx, turns, and score of the one
  which wins first"
  (var minturns 1000)
  (var winningscore 0)
  (var winningidx 0)
  (icollect [i t (ipairs inputs)]
            (let [(turns score) (iterate-board t (. inputs :nums))]
              (when (< turns minturns)
                ;(print "new winner: " turns)
                (set winningidx i)
                (set minturns turns)
                (set winningscore score))))
  (values winningidx minturns winningscore))

(fn find-loser [ inputs ]
  "Iterates over all boards and returns the idx, turns, and score of the one
  which wins last"
  (var maxturns 0)
  (var losingscore 0)
  (var losingidx 0)
  (icollect [i t (ipairs inputs)]
            (let [(turns score) (iterate-board t (. inputs :nums))]
              (when (> turns maxturns)
                ;(print "new loser " turns)
                (set losingidx i)
                (set maxturns turns)
                (set losingscore score))))
  (values losingidx maxturns losingscore))

(fn bold [x] 
  "returns a string with input text bold + resets after"
  (.. "\27[1m" x "\27[0m"))
;;;;;;;;;;;;;;
(local inp "inp/day4.txt")
(local inputs (parse-inp inp))
(let [(idx turns score) (find-winner inputs)]
  (print (.. "Day4: Part1: after " turns " turns board " idx " scores " (bold score))))
(local inputs (parse-inp inp))
(let [(idx turns score) (find-loser inputs)]
  (print (.. "Day4: Part2: after " turns " turns board " idx " scores " (bold score))))
