; fennel min heap
; Based on http://lua-users.org/lists/lua-l/2007-07/msg00482.html
; but modified to keep keys associated with values so I can reinsert
; the same thing multiple times

(local assert assert)
(local setmetatable setmetatable)

(local M {})
(fn parent-idx [n] (/ (- n (% n 2)) 2))

(fn push [heep k v]
  (assert (not= v nil) "cannot push nil")
  (local h heep.heap)
  (var n (+ 1 (length h))) ; node position in heap array (leaf)
  (var p (parent-idx n)) ; parent position in heap array
  (tset h n [k v]) ; insert at a leaf
  (while (and (> n 1) (> (. h p 2) v))
    (do ; climb heap?
      (local hp (. h p))
      (tset h p (. h n))
      (tset h n hp)
      (set n p)
      (set p (parent-idx n))
    )))

(fn peek [heep]
  ;(local t heep.nodes)
  (local h heep.heap)
  (local s (length h))
  (assert (> s 0) "cannot peek from empty heap")
  (. h 1)) ; -- min (heap root)

(fn pop [heep]
  (local h heep.heap)
  (var s (length h))
  (assert (> s 0) "cannot pop from empty heap")
  (local res (. h 1)) ; -- min (heap root)
  (local v (. h s 2))
  (tset h 1 (. h s)) ;-- move leaf to root
  (tset h s  nil) ; -- remove leaf
  (set s (- s 1))
  (var n 1) ; -- node position in heap array
  (var p (* 2 n)) ; -- left sibling position
  (when (and (> s p) (> (. h p 2) (. h (+ 1 p) 2)))
    (set p (+ p 1))) ;  -- right sibling position

  (while (and (>= s p) (< (. h p 2) v)) 
    (do ; descend heap?
      ;(print "pop n=" n " p=" p " v=" v)
      (local hp (. h p))
      (tset h p (. h n))
      (tset h n hp)
      (set n  p)
      (set p (* 2 n))
      (when (and (> s p) (> (. h p 2) (. h (+ p 1) 2)))
        (set p (+ p 1)))))
  res)

(fn isempty? [h] (= (. h :heap 1) nil))

(fn M.new []
  (var res {:heap  {}}); :nodes {}})
  (setmetatable res
                {:__index {: push : peek : pop : isempty?}})
  res)

M

