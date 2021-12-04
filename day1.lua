--inp = "inp/day1_test.txt"
inp = "inp/day1.txt"
last_number = 99999
incs = 0
values = {}

for line in io.lines(inp) do
  ln = tonumber(line)
  table.insert(values, ln)
  if ln > last_number then incs = incs + 1 end
  last_number = ln
end

print("Day1: Part1: " .. incs)

incs = 0
for i,val in ipairs(values) do
  if i > 3 then
    --print(i, "-"..values[i-3], "+"..val, val - values[i-3])
    if val > values[i-3] then
      incs = incs + 1
      --print("<increase> rem:"..values[i-3] .. " add:" .. val)
    end
  end
end

print("Day1: Part2: " .. incs)

--Day1: Part1: 1184
--Day1: Part2: 1158
