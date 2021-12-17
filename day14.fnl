(local fennel (require :fennel))
(local utils (require :utils))

(fn parse-rule [line]
  (let [(_ _ in out) (string.find line "^(%a+) %-> (%a+)$")]
    (values in out)))
;(parse-rule "NN -> B")

(fn parse-inp [file-name]
  (var res { :template nil :rules []})
  (let [f (assert (io.open file-name))]
    (tset res :template (f:read))
    (f:read) ; blank line
    (var line (f:read))
    (while (not= nil line)
      (do 
        (let [(in out) (parse-rule line)]
          (tset res.rules in out))
        (set line (f:read)))))
  res)

(fn stringdex [str i] (string.sub str i i))
;(stringdex "asdfg" 1)

(fn step [line rules]
  "Just iterate through and do the obvious thing"
  (var res [])
  (for [i 1 (- (length line) 1)]
    (local pair (.. (stringdex line i) (stringdex line (+ 1 i))))
    (table.insert res (stringdex line i))
    (table.insert res (. rules pair)))
  (table.insert res (stringdex line (length line)))
  (table.concat res ""))

(fn count-letters [line]
  (var tots {})
  (for [i 1 (length line)]
    (utils.tab-inc tots (stringdex line i)))
  tots)

(fn get-diff [counts]
  "Returns the largest number - the smallest"
  (var max 0)
  (var min (^ 2 99))
  (each [_ v (pairs counts)]
    (when (> v max) (set max v))
    (when (< v min) (set min v)))
  (- max min))

(fn res-pairs [ac b]
  (let [a (stringdex ac 1)
        c (stringdex ac 2)] (values (.. a b) (.. b c))))
;(res-pairs "NN" "H")

(fn step-pair-counts [inp-pairs rules]
  (var res (utils.clone inp-pairs))
  (each [ac cnt (pairs inp-pairs)]
    (let [(ab bc) (res-pairs ac (. rules ac))]
      (utils.tab-inc res ab cnt)
      (utils.tab-inc res bc cnt)))
  res)

(fn line-to-pair-counts [line]
  (var res {})
  (for [i 1 (- (length line) 1)]
    (local pair (.. (stringdex line i) (stringdex line (+ 1 i))))
    (utils.tab-inc res pair))
  res)
    
; Count the first letter of all combos + add one for the last letter
(fn count-letters-pairs [pair-cnts last-letter]
  (var res {})
  (each [ab cnt (pairs pair-cnts)]
    (utils.tab-inc res (stringdex ab 1) cnt))
  (utils.tab-inc res last-letter)
  res)


;;;;;;;;;;;;;;
;(local inp-file "inp/day14_test.txt")
(local inp-file "inp/day14.txt")
(local inputs (parse-inp inp-file))
;(comment ( ; Old way, didn't really work for part 2...
           ; take some lanternfish inspiration instead
;(var stepped inputs.template)
;(for [i 1 10]
;  (set stepped (step stepped inputs.rules)))
;(local counts (count-letters stepped))
;(local pt1-res (get-diff counts))))

(var inp-pairs (line-to-pair-counts inputs.template))

(for [i 1 10]
  (set inp-pairs (step-pair-counts inp-pairs inputs.rules)))
(local pt1-res (get-diff (count-letters-pairs inp-pairs (stringdex inputs.template (length inputs.template)))))
(print (.. "Day14: Part1: after 10 steps the diff of most/least common elements is " (utils.bold pt1-res)))

(for [i 11 40]
  (set inp-pairs (step-pair-counts inp-pairs inputs.rules)))
(local pt2-res (get-diff (count-letters-pairs inp-pairs (stringdex inputs.template (length inputs.template)))))
; 3694763512494 too high!
(print (.. "Day14: Part2: after 40 steps the diff of most/least common elements is " (utils.bold pt2-res)))
