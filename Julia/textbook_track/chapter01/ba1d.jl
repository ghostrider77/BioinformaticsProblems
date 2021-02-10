# Find All Occurrences of a Pattern in a String

function find_pattern_occurrences(text, pattern)
    k = length(pattern)
    matching_indices = Int64[]
    for ix in 1:(length(text)-k+1)
        substring = text[ix:ix+k-1]
        if substring == pattern
            push!(matching_indices, ix - 1)
        end
    end
    matching_indices
end


function main()
    pattern = readline()
    genome = readline()
    result = find_pattern_occurrences(genome, pattern)
    println(join(result, " "))
end


main()
