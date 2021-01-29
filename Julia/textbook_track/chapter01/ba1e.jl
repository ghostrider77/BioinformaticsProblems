# Find Patterns Forming Clumps in a String

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


function compute_frequencies(text, k)
    frequencies = zeros(Int32, 4 ^ k)
    for ix in 1:(length(text)-k)
        pattern = text[ix:ix+k-1]
        code = pattern_to_number(pattern)
        frequencies[code+1] += 1
    end
    frequencies
end


function collect_clump_forming_k_mers(clump_array, k)
    k_mers_in_clump = String[]
    for (ix, isclump) in enumerate(clump_array)
        if isclump
            k_mer = number_to_pattern(ix - 1, k)
            push!(k_mers_in_clump, k_mer)
        end
    end
    k_mers_in_clump
end


function find_clumps_in_text(genome, k, l, t)
    clump_array = falses(4 ^ k)
    frequencies = compute_frequencies(genome[1:l], k)
    for (ix, freq) in enumerate(frequencies)
        if freq >= t
            clump_array[ix] = true
        end
    end

    for ix in 2:(length(genome)-l)
        start_k_mer = genome[ix-1:ix-2+k]
        start_code = pattern_to_number(start_k_mer)
        frequencies[start_code+1] -= 1
        end_k_mer = genome[ix+l-k:ix+l-1]
        end_code = pattern_to_number(end_k_mer)
        frequencies[end_code+1] += 1
        if frequencies[end_code+1] >= t
            clump_array[end_code+1] = true
        end
    end

    collect_clump_forming_k_mers(clump_array, k)
end


function main()
    genome = readline()
    k, l, t = convert_to_intlist(readline())
    result = find_clumps_in_text(genome, k, l, t)
    println(join(result, " "))
end


main()
