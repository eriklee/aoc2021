(local fennel (require :fennel))
(local utils (require :utils))

(fn parse-inp [filename]
  "Returns a table of syntax strings (not massively helpful tbh...)"
  (var tbl {})
  (each [l (io.lines filename)] (table.insert tbl l))
  tbl)

(fn syn-score-char [c]
  "Returns the syntax error score for the char"
  (if (= c ")") 3
    (= c "]") 57
    (= c "}") 1197
    (= c ">") 25137
    0))

(fn opens? [c]
  "Is c a char which begins a scope?"
  (or (= c "(")
      (= c "[")
      (= c "{")
      (= c "<")))

(fn matches? [o c]
  "Does opening char match closing char?"
  (or (and (= o "(") (= c ")"))
      (and (= o "[") (= c "]"))
      (and (= o "{") (= c "}"))
      (and (= o "<") (= c ">"))))

(fn find-bad-char [line]
  "Returns 2 values - bad char + remaining stack"
  (var stack {})
  (var bad-char nil)
  (for [i 1 (length line)]
    (local c (string.char (string.byte line i)))
    (if (opens? c) (table.insert stack c)
      (matches? (table.remove stack) c) []
      (set bad-char (or bad-char c))))
  (values bad-char stack))

(fn part1-score [inp]
  (accumulate [tot 0
               _ line (ipairs inp)]
              ;(do (print line (find-bad-char line))
              (+ tot (syn-score-char (find-bad-char line)))))

(fn get-remains [inp]
  "Returns a table with just the partial stacks"
  (var rem {})
  (each [_ line (ipairs inp)]
    (let [(bc stack) (find-bad-char line)]
      (when (and (= bc nil) (> (length stack) 0))
        (table.insert rem stack))))
  rem)

(fn comp-score-char [c]
  "Returns the completion score for the char"
  (if (= c "(") 1
    (= c "[") 2
    (= c "{") 3
    (= c "<") 4
    0))

(fn score-remains [rem]
  "Calculates the score for the completion on a single line
  The minimal completion is just popping everything back off the stack"
  (var res 0)
  (while (> (length rem) 0)
    (set res (+ (* 5 res) (comp-score-char (table.remove rem)))))
  res)

(fn tbl-median-idx [tbl]
  (local sz (length tbl))
  (math.ceil (/ sz 2)))

(fn calc-total-remains-score [rems]
  (var scores {})
  (each [_ rm (ipairs rems)]
    (table.insert scores (score-remains rm)))
  (table.sort scores)
  (local med (tbl-median-idx scores))
  (. scores med))


;;;;;;;;;;;;;;;;;;;;;;;
(local inp-file "inp/day10.txt")
;(local inp-file "inp/day10_test.txt")
(var inp (parse-inp inp-file))

(local part1 (part1-score inp))
(print (.. "Day10: Part1: Total Syntax error score: " (utils.bold part1)))

(local part2 (calc-total-remains-score (get-remains inp)))
(print (.. "Day10: Part2: median of completion scores: " (utils.bold part2)))
