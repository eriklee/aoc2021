(local fennel (require :fennel))
(local heap (require :heap))

(var h (heap.new))

(h:push :a 2)
(h:push :b 3)
(h:push :c 4)
(h:push :d 1)
(h:push :a 0)

(for [i 0 10] (h:push i (math.random)))

;(print (fennel.view h))
(var last -1)
(while (not (h:isempty?))
  (let [[k v] (h:pop)]
    (print "pop " (fennel.view k) v)
    (assert (< last v) "Heap popping didn't work?!")
    (set last v)))
