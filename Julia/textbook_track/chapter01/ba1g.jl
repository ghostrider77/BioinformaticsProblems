# Compute the Hamming Distance Between Two Strings

calc_hamming_distance(s1, s2) = count(((c1, c2),) -> c1 != c2, zip(s1, s2))


function main()
    dna1 = readline()
    dna2 = readline()
    result = calc_hamming_distance(dna1, dna2)
    println(result)
end


main()
