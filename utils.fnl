(fn bold [x] 
  "returns a string with input text bold + resets after"
  (.. "\27[1m" x "\27[0m"))

{:bold bold}
