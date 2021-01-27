# Implement PatternToNumber

const NUCLEOTIDE_ORDER = Dict{Char, Int64}('A' => 0, 'C' => 1, 'G' => 2, 'T' => 3)


pattern_to_number(pattern) = foldl((acc, nucleotide) -> 4 * acc + NUCLEOTIDE_ORDER[nucleotide], pattern; init=0)


function main()
    dna = readline()
    result = pattern_to_number(dna)
    println(result)
end


main()
