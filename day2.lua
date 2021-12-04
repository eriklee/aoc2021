inp = "inp/day2.txt"

horizontal = 0
depth = 0

for line in io.lines(inp) do
  words = {}
  for word in string.gmatch(line, "%w+") do
    table.insert(words, word)
  end
  dir = words[1]
  count = words[2]

  if dir == "forward" then
    horizontal = horizontal + count
  elseif dir == "down" then
    depth = depth + count
  elseif dir == "up" then
    depth = depth - count
  else
    print("oh no! "..dir.." "..count)
  end
end

print("Day2: Part1: h*d=" ..(horizontal * depth))

horizontal = 0
depth = 0
aim = 0

for line in io.lines(inp) do
  words = {}
  for word in string.gmatch(line, "%w+") do
    table.insert(words, word)
  end
  dir = words[1]
  count = words[2]

  if dir == "forward" then
    horizontal = horizontal + count
    depth = depth + (aim * count)
  elseif dir == "down" then
    aim = aim + count
  elseif dir == "up" then
    aim = aim - count
  else
    print("oh no! "..dir.." "..count)
  end
end

print("Day2: Part2: h*d=" ..(horizontal * depth))

--Day2: Part1: h*d=1893605
--Day2: Part2: h*d=2120734350
