(local fennel (require :fennel))
(local utils (require :utils))

(fn parse-inp [filename]
  ""
  (var tbl {})
  (each [ line (io.lines filename)]
    (let [(_ _ fst snd) (string.find line "([^|]*) | (.*)")]
      (local signals (icollect [ w (string.gmatch fst "%S+")] w))
      (local digits (icollect [w (string.gmatch snd "%S+")] w))
      (table.insert tbl {:signals signals :out-digits digits})))
  tbl)

(fn count-1478 [inl]
  "counts the number of out digits of length 2,3,4,7"
  (accumulate [cnt 0
               _ w (ipairs (. inl :out-digits))]
              (if 
                (= 2 (length w)) ; 1
                (+ cnt 1)
                (= 3 (length w)) ; 7
                (+ cnt 1)
                (= 4 (length w)) ; 4
                (+ cnt 1)
                (= 7 (length w)) ; 8
                (+ cnt 1)
                cnt)))

(fn count-all [inp]
  "counts the number of out digits for part 1 on all input"
  (accumulate [tot 0
               _ lr (ipairs inp)]
             (+ tot (count-1478 lr)))) 

(fn intersect [w1 w2]
  "returns the intersection of 2 strings"
  (var tbl {})
  (for [i 1 (length w1)]
    (tset tbl (string.byte w1 i) true))
  (var out {})
  (for [i 1 (length w2)]
    (when (not= nil (. tbl (string.byte w2 i)))
      (tset out (string.byte w2 i) true)))
  out)

(fn tlength [t]
  "Returns the size of a table even if it has gaps or isn't an array..."
  (accumulate [sz 0
               _ _ (pairs t)]
              (+ 1 sz)))

(fn invert-table [int]
  "Flips a table around so all keys become values and vice-versa"
  (var out {})
  (each [k v (pairs int)] (tset out v k))
  out)

(fn sort-str [ins]
  "Returns a string with the characters sorted"
  (var t {})
  (for [i 1 (length ins)]
    (table.insert t (string.char (string.byte ins i))))
  (table.sort t)
  (var r "") ; lol, (.. (unpack ...)) doesn't work.... :(
  (each [_ c (ipairs t)] (set r (.. r c)))
  r)

(fn decode-signals [inl]
  "maps all signals to numbers and returns the decoder table"
  (var decodings {})
  (var fives {})
  (var sixes {})
  ; first match the signals for 1,4,7,8 due to the uniq segment counts
  (each [_ w (ipairs (. inl :signals))]
    (if 
      (= 2 (length w)) ; 1
      (tset decodings 1 (sort-str w))
      (= 3 (length w)) ; 7
      (tset decodings 7 (sort-str w))
      (= 4 (length w)) ; 4
      (tset decodings 4 (sort-str w))
      (= 7 (length w)) ; 8
      (tset decodings 8 (sort-str w))
      (= 5 (length w)) ; 2,3,5
      (table.insert fives w)
      (= 6 (length w)) ; 2,3,5
      (table.insert sixes w)
      (print "erm, what" w)
      ))
  ; Now thanks to looking at the numbers, distinguishing the rest can
  ; be done by overlapping the known numbers and either comparing for
  ; equality or the size. I'd half consider doing this as bytes and 
  ; use & and popcnt and so on, but that seems a bit much...
  (each [_ w (ipairs fives)]
    (if
      (= 2 (tlength (intersect (. decodings 1) w))) ; 1 overlaps with 3
      (tset decodings 3 (sort-str w))
      (= 2 (tlength (intersect (. decodings 4) w))) ; |4 & 2| = 2
      (tset decodings 2 (sort-str w))
      (= 3 (tlength (intersect (. decodings 4) w))) ; |4 & 2| = 2
      (tset decodings 5 (sort-str w))
      (print "failed to identify a 5s: " w)
      )
    )
  (each [_ w (ipairs sixes)]
    (if
      (= 4 (tlength (intersect (. decodings 4) w))) ; 4 overlaps with 9
      (tset decodings 9 (sort-str w))
      (= 2 (tlength (intersect (. decodings 1) w))) ; |1 & 0| = 1
      (tset decodings 0 (sort-str w))
      (= 5 (tlength (intersect (. decodings 5) w))) ; |5 & 6| = 5
      (tset decodings 6 (sort-str w))
      (print "failed to identify a 5s: " w)
      )
    )
  ; we want to look up by code rather than digit
  (invert-table decodings))

(fn read-outputs [inp decodings]
  "Returns the decoded output"
  (accumulate [r 0
               _ out (ipairs inp.out-digits)]
              (+ (* r 10) (. decodings (sort-str out)))))

(fn solve-part2 [inp]
  (accumulate [tot 0
               _ inl (ipairs inp)]
              (do (local dec (decode-signals inl))
              (+ tot (read-outputs inl dec)))))


;;;;;;;;
(local inp-file "inp/day8.txt")
;(local inp-file "inp/day8_test.txt")
(var inp (parse-inp inp-file))

(print (.. "Day8: Part1: " (utils.bold (count-all inp)) " instances of 1,4,7,8"))
(print (.. "Day8: Part2: Sum of all decoded outputs: " (utils.bold (solve-part2 inp))))
