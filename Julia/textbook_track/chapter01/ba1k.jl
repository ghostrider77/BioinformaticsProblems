# Generate the Frequency Array of a String

const NUCLEOTIDE_ORDER = Dict{Char, Int64}('A' => 0, 'C' => 1, 'G' => 2, 'T' => 3)


pattern_to_number(pattern) = foldl((acc, nucleotide) -> 4 * acc + NUCLEOTIDE_ORDER[nucleotide], pattern; init=0)


function computing_frequencies(text, k)
    frequencies = zeros(Int32, 4 ^ k)
    for ix in 1:(length(text)-k+1)
        pattern = text[ix:ix+k-1]
        code = pattern_to_number(pattern)
        frequencies[code+1] += 1
    end
    frequencies
end


function main()
    dna = readline()
    k = parse(Int64, readline())
    result = computing_frequencies(dna, k)
    println(join(result, " "))
end


main()
