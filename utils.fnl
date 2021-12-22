(var M {})

(fn M.bold [x]
  "returns a string with input text bold + resets after"
  (.. "\27[1m" x "\27[0m"))

(fn M.boldred [x]
  "returns a string with input text bold + resets after"
  (.. "\27[31;1m" x "\27[0m"))

(fn M.boldgreen [x]
  "returns a string with input text bold + resets after"
  (.. "\27[32;1m" x "\27[0m"))

(fn M.tab-inc [tbl k ?v]
  "Increment value at k (by v or 1) (or sets if value is nil)"
  (tset tbl k (+ (or ?v 1) (or (?. tbl k) 0))))

(fn M.sum-table [tab]
  "Returns the sum of all numeric values in the table"
  (var sum 0)
  (each [i x (ipairs tab)]
    (when (= "number" (type x)) (set sum (+ sum x))))
  sum)

(fn M.table-append [tbl newvals]
  "Appends the second table to the first"
  (for [i 1 (length newvals)]
    (table.insert tbl (. newvals i))))

(fn M.tlength [t]
  "Returns the size of a table even if it has gaps or isn't an array..."
  (accumulate [sz 0
               _ _ (pairs t)]
              (+ 1 sz)))

; clones a table of values (shallowly, and only for listy tables)
(fn M.clone [tbl] (var t2 []) (each [_ v (ipairs  tbl)] (table.insert t2 v)) t2)

; deeply clones a listy table of values
(fn M.deepclone [tbl] 
  (var t2 [])
  (each [k v (pairs  tbl)]
    (if (= "table" (type v))
      (tset t2 k (M.deepclone v))
      (tset t2 k v)))
  t2)

(fn M.table-reverse [tbl]
  "Returns a new table which is the first one reversed"
  (var res [])
  (for [i 1 (length tbl)]
    (table.insert res (table.remove tbl)))
  res)

M
