# Implement DistanceBetweenPatternAndStrings

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


function main()
    pattern = readline()
    k = length(pattern)
    texts = split(readline())
    result = distance_between_pattern_and_collection_of_texts(texts, pattern, k)
    println(result)
end


main()
