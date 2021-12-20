(local fennel (require :fennel))
(local utils (require :utils))

; some delightful eval cheating here
(fn parse-sn [line]
  (let [pline (line:gsub "," " ")]
    (fennel.eval pline)))

;(local testline "[[[[1,5],[2,8]],[[4,1],3]],[[1,[6,3]],[[9,7],8]]]")
(local testline "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]")
(parse-sn testline)

(fn parse-inp [filename]
  "returns a table of all input snailfish numbers"
  (var tbl [])
  (each [line (io.lines filename)]
      (table.insert tbl (parse-sn line)))
  tbl)

(fn isnumber [x] (= "number" (type x)))

(fn ppsn [sn]
  (var res [])
  (if (isnumber sn)
    (table.insert res sn)
    (do
      (table.insert res "[")
      (table.insert res (ppsn (. sn 1)))
      (table.insert res ",")
      (table.insert res (ppsn (. sn 2)))
      (table.insert res "]")))
  (table.concat res ""))

(fn split-n [sn]
  "returns the pair which is the result of splitting the number"
  (assert (isnumber sn) "split-n called on a pair")
  [(math.floor (/ sn 2)) (math.ceil (/ sn 2))])

(fn explode-add-right [sn right-n]
  ; recurse down the lhs until we find a number we can inc
  (if 
    (isnumber (. sn 1)) (tset sn 1 (+ right-n (. sn 1)))
    (explode-add-right (. sn 1) right-n)))

; go down the tree looking for the first instance of a
; pair nested more than 4 deep (remembering the left most
; slot we'd need to add the number to).
; if we do explode pass the fact back up until we can find a right
; slot to increment (and then pass back another sentinel)
; if we don't, eh
; state has the left index and slot number (maybe should have more, but eh)
(fn try-explode [sn lvl st]
  (var acted false)
  (var right-n nil)

  ;(print "attempting lvl=" lvl "left-spot=" (fennel.view st.left-spot) "l-s-i=" st.left-spot-idx)
  ;(print (ppsn sn))

  (if 
    ; we have a new left side, but can't recurse
    (isnumber (. sn 1)) (do (tset st :left-spot sn) (tset st :left-spot-idx 1)) 
    ; we're at level 4 (and our child must be a pair)
    (= lvl 4) (do 
                (when (not= nil st.left-spot)
                  (tset st.left-spot ; inc the left-spot by the left pair if possible
                        st.left-spot-idx 
                        (+ (. sn 1 1)
                           (. st.left-spot st.left-spot-idx))))
                (set right-n (. sn 1 2)) ; remember the right hand pair
                (tset sn 1 0) ; set the slot to 0
                (set acted true))
    ; didn't explode, recurse down
    (do 
      (local te-res (try-explode (. sn 1) (+ 1 lvl) st))
      (match te-res
        ; we had an exp on the left, but it's resolved
        {:st :done} (set acted true) 
        ; we had an exp on the left, and we must use the rhs
        {:st :incomplete :n n} (do (set acted true) (set right-n n))
        ; nothing happend, keep looking
        nil nil)))
  (if
    ; we need to add right-n to the next child
    (not= nil right-n) (do
                         ; the right side is just a number we can add to
                         (if (isnumber (. sn 2)) (do (tset sn 2 (+ (. sn 2) right-n))
                                                   (set right-n nil))
                           ; the right side is a pair we can use (this must succeed)
                           (do (explode-add-right (. sn 2) right-n)
                             (set right-n nil))))
    ; we haven't done anything yet, just try to find an explosion on the rhs
    ; but the rhs is a number so can't explode - just update the left-spot
    (isnumber (. sn 2)) (do (tset st :left-spot sn) (tset st :left-spot-idx 2))
    ; we're at level 4, we haven't acted yet, and the rhs is a pair - explode
    (and (not acted) (= lvl 4)) (do
                (tset st.left-spot st.left-spot-idx ; we know this is safe
                      (+ (. sn 2 1) (. st.left-spot st.left-spot-idx)))
                (set right-n (. sn 2 2))
                (tset sn 2 0)
                (set acted true))
    ; we haven't found anything, we're not deep enough, recurse
    (not acted) (do
      (local te-res (try-explode (. sn 2) (+ 1 lvl) st))
      (match te-res
        ; we had an exp on the right, but it's resolved
        {:st :done} (set acted true) 
        ; we had an exp on the right, and we must use the rhs
        {:st :incomplete :n n} (do (set acted true) (set right-n n))
        ; nothing happend, keep looking
        nil nil)))
  ; time to return - 3 cases
  ; 1) we exploded something but have resolved it
  ; 2) we exploded something and still need to inc rhs
  ; 3) we did nothing
  ;(print "explosion result level=" lvl " acted=" acted " right-n=" right-n)
  (if
    (and acted (= nil right-n)) {:st :done}
    acted {:st :incomplete :n right-n}
    nil))

(fn init-te-state [] {:left-spot nil :left-spot-idx nil})
;(local sn (parse-sn "[[1,2],[[3,4],5]]"))
;(try-explode sn 1 (init-te-state))
;(local sn (parse-sn "[[[[[9,8],1],2],3],4]"))
;(try-explode sn 1 (init-te-state))
;(local sn (parse-sn "[7,[6,[5,[4,[3,2]]]]]"))
;(try-explode sn 1 (init-te-state))
;(local sn (parse-sn "[[6,[5,[4,[3,2]]]],1]"))
;(try-explode sn 1 (init-te-state))
;(local sn (parse-sn "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"))
;(try-explode sn 1 (init-te-state))
;(local sn (parse-sn "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))
;(try-explode sn 1 (init-te-state))

(fn try-split [sn]
  (assert (not (isnumber sn)) "split called on a reg-num")
  (var acted false)
  (if
    ; lhs is a number and it's too high
    (and (isnumber (. sn 1)) (> (. sn 1) 9))
      (do (set acted true) (tset sn 1 (split-n (. sn 1))))
    ; lhs is a pair which we were able to split
    (and (not (isnumber (. sn 1))) (try-split (. sn 1))) (set acted true) 
    ; rhs is a number and it's too high
    (and (isnumber (. sn 2)) (> (. sn 2) 9))
      (do (set acted true) (tset sn 2 (split-n (. sn 2))))
    ; rhs is a pair which we were able to split
    (and (not (isnumber (. sn 2))) (try-split (. sn 2))) (set acted true))
  acted)

;(local sn (parse-sn "[[[[0,7],4],[15,[0,13]]],[1,1]]"))
;(try-split sn)


(fn reduce [sn]
  ;(print :reducing (ppsn sn))
  (if 
    (not= nil (try-explode sn 1 (init-te-state)))
    (do ;(print "exploded: " (ppsn sn))
      (reduce sn))
    (if (try-split sn)
      (do ;(print "split:    " (ppsn sn))
        (reduce sn)))))

;(local sn (parse-sn "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))
;(reduce sn)

(fn add [sn1 sn2]
  (var res (utils.deepclone [sn1 sn2]))
  (reduce res)
  res)

;(local sn1 (parse-sn "[[[[4,3],4],4],[7,[[8,4],9]]]"))
;(local sn2 (parse-sn "[1,1]"))
;(add sn1 sn2)

(fn accum-lines [lines]
  (var res (. lines 1))
  (for [i 2 (length lines)]
    ;(print "  " (ppsn res))
    ;(print "+ " (ppsn (. lines i)))
    (set res (add res (. lines i)))
    ;(print "= " (ppsn res))
    ;(print ""))
    )
  res)

(fn magnitude [sn]
  (if (isnumber sn)
    sn
    (do
      (local lm (magnitude (. sn 1)))
      (local rm (magnitude (. sn 2)))
      (+ (* 3 lm) (* 2 rm)))))

;(assert (= 143 (magnitude (parse-sn "[[1,2],[[3,4],5]]"))))
;(assert (= 1384 (magnitude (parse-sn "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))))
;(assert (= 445 (magnitude (parse-sn "[[[[1,1],[2,2]],[3,3]],[4,4]]"))))
;(assert (= 791 (magnitude (parse-sn "[[[[3,0],[5,3]],[4,4]],[5,5]]"))))
;(assert (= 1137 (magnitude (parse-sn "[[[[5,0],[7,4]],[5,5]],[6,6]]"))))
;(assert (= 3488 (magnitude (parse-sn "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))))

(fn find-highest-sum [inp]
  (var max 0)
  (for [i 1 (length inp)]
    (for [j (+ 1 i) (length inp)]
      (local sum (magnitude (add (. inp i) (. inp j))))
      (when (> sum max)
        ;(print "New max=" sum " from i=" i "+ j=" j)
        (set max sum))
      (local sum (magnitude (add (. inp j) (. inp i))))
      (when (> sum max)
        ;(print "New max=" sum " from j=" j "+ i=" i)
        (set max sum))))
  max)

;;;;;;;;
(local inp-file "inp/day18.txt")
;(local inp-file "inp/day18_test1.txt")
;(local inp-file "inp/day18_test2.txt")
(var inp (parse-inp inp-file))

(local part1sn (accum-lines inp))
(local part1 (magnitude part1sn))
(print (.. "Day18: Part1: magnitude of sn is: " (utils.bold part1)))

(var inp (parse-inp inp-file))
(local part2 (find-highest-sum inp))
(print (.. "Day18: Part2: highest magnitude of the input numbers is: " (utils.bold part2)))
(local res (add (. inp 9) (. inp 1)))
