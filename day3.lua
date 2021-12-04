--Day3: Part1: γ*ε        = 1997414
--
inp = "inp/day3.txt"

values = {}

-- for every 1 add one, subtract 1 for 0
-- if values[i] is negative, most common was 0,
-- otherwise it was 1
for line in io.lines(inp) do
  for i = 1,#line do
    if string.sub(line,i,i) == "1" then
      values[i] = 1 + (values[i] or 0)
    else
      values[i] = -1 + (values[i] or 0)
    end
  end
end

gamma = 0
epsilon = 0

for i, val in ipairs(values) do
  if val < 0 then
    gamma = gamma * 2 + 0
    epsilon = epsilon * 2 + 1
  elseif val > 0 then
    gamma = gamma * 2 + 1
    epsilon = epsilon * 2 + 0
  end
end

--print("γ: "..gamma .. " ε: " .. epsilon)
print("Day3: Part1: γ*ε	= " .. gamma * epsilon)

-- part 2
vals = {}
for line in io.lines(inp) do
  table.insert(vals, line)
end

-- finds the most common value of the elements in the table
-- at position i
function mcvf(tab, i)
  indicator = 0
  for _, v in ipairs(tab) do
    vi = string.sub(v,i,i)
    if vi == "1" then indicator = indicator + 1
    elseif vi == "0" then indicator = indicator - 1
    end
  end
  if indicator >= 0 then return 1 else return 0 end
end

-- oxygen_generator_rating
idx = 1
while #vals > 1 do
  mcv = mcvf(vals, idx)
  for i = #vals, 1, -1 do
    vi = tonumber(string.sub(vals[i],idx,idx))
    if vi ~= mcv then table.remove(vals, i) end
  end
  idx = idx + 1
end

oxygen_gen = vals[1]
--print("oxygen_generator_rating: "..tonumber(oxygen_gen, 2))

-- co2_scrubber_rating
vals = {}
for line in io.lines(inp) do
  table.insert(vals, line)
end

idx = 1
while #vals > 1 do
  mcv = mcvf(vals, idx)
  for i = #vals, 1, -1 do
    vi = tonumber(string.sub(vals[i],idx,idx))
    if vi == mcv then 
      table.remove(vals, i)
    end
  end
  idx = idx + 1
end
co2_rating = vals[1]
--print("co2_scrubber_rating: "..tonumber(co2_rating, 2))
print("Day3: Part2:  o2 * co2	= " .. tonumber(oxygen_gen, 2) * tonumber(co2_rating, 2))
