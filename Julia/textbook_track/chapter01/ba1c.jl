# Find the Reverse Complement of a String

const NUCLEOTIDE_COMPLEMENTS = Dict('A' => 'T', 'C' => 'G', 'T' => 'A', 'G' => 'C')


calc_reverse_complement(genome) = join(map(n -> NUCLEOTIDE_COMPLEMENTS[n], Iterators.reverse(genome)), "")


function main()
    dna = readline()
    result = calc_reverse_complement(dna)
    println(result)
end


main()
