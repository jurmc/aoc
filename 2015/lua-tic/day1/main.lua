-- title:  aoc-2015-day1
-- author: jurmcc
-- script: lua

include "test_input_day01"
include "input_day01"

test_expected = -3
cls(0)

function aoc_resolve(data)
    acc = 0
    for c in data:gmatch(".") do
        if c == "(" then
            acc = acc +1
        elseif c == ")" then
            acc = acc -1
        end
    end
    return acc
end

test_resolved = aoc_resolve(test_data)
resolved = aoc_resolve(data)

function TIC()
    print("test_data: " .. test_data, 0, 0, 12)
    print("test_expected: " .. test_expected, 0, 8, 12)
    print("test_resolved: " .. test_resolved, 0, 16, 12)

    print("resolved: " .. resolved, 0, 24, 12)
end

