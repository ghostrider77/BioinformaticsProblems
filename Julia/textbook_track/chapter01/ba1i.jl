# Find the Most Frequent Words with Mismatches in a String

const NUCLEOTIDES = ('A', 'C', 'G', 'T')
const NUCLEOTIDE_ORDER = Dict{Char, Int64}('A' => 0, 'C' => 1, 'G' => 2, 'T' => 3)


convert_to_intlist(line) = map(x -> parse(Int, x), split(line))


pattern_to_number(pattern) = foldl((acc, nucleotide) -> 4 * acc + NUCLEOTIDE_ORDER[nucleotide], pattern; init=0)


function number_to_pattern(encoding, k)
    pattern = Char[]
    for _ in 1:k
        encoding, remainder = divrem(encoding, 4)
        push!(pattern, NUCLEOTIDES[remainder+1])
    end
    join(reverse(pattern))
end


function calc_frequencies_for_approximate_occurrences(text, k, d)
    frequencies = zeros(Int32, 4 ^ k)
    for ix in 1:(length(text)-k)
        exact_pattern = text[ix:ix+k-1]
        for pattern in generate_neighbourhood(exact_pattern, d)
            code = pattern_to_number(pattern)
            frequencies[code+1] += 1
        end
    end
    frequencies
end


function calc_immediate_neighbours(pattern)
    neighbours = Set{String}()
    for (ix, letter) in enumerate(pattern)
        for nucleotide in NUCLEOTIDES
            if nucleotide != letter
                pattern_array = collect(pattern)
                pattern_array[ix] = nucleotide
                push!(neighbours, join(pattern_array))
            end
        end
    end
    neighbours
end


function generate_neighbourhood(text, d)
    neighbourhood = Set{String}([text])
    for _ in 1:d
        neighbours_of_neighbours = Set{String}()
        for pattern in neighbourhood
            immediate_neighbours = calc_immediate_neighbours(pattern)
            union!(neighbours_of_neighbours, immediate_neighbours)
        end
        union!(neighbourhood, neighbours_of_neighbours)
    end
    neighbourhood
end


function most_frequent_approximate_k_mers(text, k, d)
    frequencies = calc_frequencies_for_approximate_occurrences(text, k, d)
    max_occurrence = maximum(frequencies)
    patterns = String[]
    for (ix, freq) in enumerate(frequencies)
        if freq == max_occurrence
            pattern = number_to_pattern(ix - 1, k)
            push!(patterns, pattern)
        end
    end
    patterns
end


function main()
    text = readline()
    k, d = convert_to_intlist(readline())
    result = most_frequent_approximate_k_mers(text, k, d)
    println(join(result, " "))
end


main()
