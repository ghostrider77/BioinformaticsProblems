# Find a Median String
using IterTools

const NUCLEOTIDES = ('A', 'C', 'G', 'T')


calc_hamming_distance(s1, s2) = count(((c1, c2),) -> c1 != c2, zip(s1, s2))


function distance_between_pattern_and_text(text, pattern, k)
    mindist = k
    for ix in 1:(length(text)-k+1)
        kmer = text[ix:ix+k-1]
        distance = calc_hamming_distance(pattern, kmer)
        if distance < mindist
            mindist = distance
        end
    end
    mindist
end


distance_between_pattern_and_collection_of_texts(texts, pattern, k) =
    foldl((distance, text) -> distance + distance_between_pattern_and_text(text, pattern, k), texts; init=0)


function calc_median_string(texts, k)
    nr_texts = length(texts)
    minimum_distance = nr_texts * k
    median_string = ""
    for pattern in imap(join, Iterators.product(ntuple(_ -> NUCLEOTIDES, k)...))
        distance = distance_between_pattern_and_collection_of_texts(texts, pattern, k)
        if distance < minimum_distance
            minimum_distance = distance
            median_string = pattern
        end
    end
    median_string
end


function main()
    k = parse(Int64, readline())
    texts = readlines()
    result = calc_median_string(texts, k)
    println(result)
end


main()
