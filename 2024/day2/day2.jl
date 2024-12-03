using DelimitedFiles;
using OffsetArrays;

function read_input(path = "input.txt")
    lines = readlines(path)

    levels = []
    for line in lines
        push!(levels, map(x -> parse(Int64, x), split(line, " ")))
    end
    levels
end

function is_safe(row, drop=false)
    rdiff = row[1:end - 1] .- row[2:end]
    max_diff = maximum(abs.(rdiff))
    safe = max_diff > 0 && max_diff <= 3 && length(unique(sign.(rdiff))) == 1

    if (!safe && drop)
        drop_rows = [OffsetArray(row)[1:end .!=i] for i in 1:length(row)]
        safe = any(map(x -> is_safe(x, false), drop_rows))
    end
    safe
end

levels = read_input();

# Part 1
safe = []
map(row -> push!(safe, is_safe(row)), levels);
println(sum(safe))

# Part 2
safe = []
map(row -> push!(safe, is_safe(row, true)), levels);
println(sum(safe))
