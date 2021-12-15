(fn bold [x]
  "returns a string with input text bold + resets after"
  (.. "\27[1m" x "\27[0m"))

(fn tab-inc [tbl k ?v]
  "Increment value at k (by v or 1) (or sets if value is nil)"
  (tset tbl k (+ (or ?v 1) (or (?. tbl k) 0))))

(fn sum-table [tab]
  "Returns the sum of all numeric values in the table"
  (var sum 0)
  (each [i x (ipairs tab)]
    (when (= "number" (type x)) (set sum (+ sum x))))
  sum)

(fn table-append [tbl newvals]
  "Appends the second table to the first"
  (for [i 1 (length newvals)]
    (table.insert tbl (. newvals i))))

(fn tlength [t]
  "Returns the size of a table even if it has gaps or isn't an array..."
  (accumulate [sz 0
               _ _ (pairs t)]
              (+ 1 sz)))

(fn clone [tbl] (var t2 []) (each [_ v (ipairs  tbl)] (table.insert t2 v)) t2)

{:bold bold :tab-inc tab-inc
 :sum-table sum-table :table-append table-append
 :tlength tlength :clone clone}
