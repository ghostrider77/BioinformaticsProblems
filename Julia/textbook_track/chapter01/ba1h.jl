# Find All Approximate Occurrences of a Pattern in a String

calc_hamming_distance(s1, s2) = count(((c1, c2),) -> c1 != c2, zip(s1, s2))


function find_approximate_pattern_occurrences(text, pattern, d)
    patternlength = length(pattern)
    matching_indices = Int64[]
    for ix in 1:(length(text)-patternlength)
        substring = text[ix:ix+patternlength-1]
        if calc_hamming_distance(substring, pattern) <= d
            push!(matching_indices, ix - 1)
        end
    end
    matching_indices
end


function main()
    pattern = readline()
    text = readline()
    d = parse(Int64, readline())
    result = find_approximate_pattern_occurrences(text, pattern, d)
    println(join(result, " "))
end


main()
