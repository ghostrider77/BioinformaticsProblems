# Implement MotifEnumeration

const NUCLEOTIDES = ('A', 'C', 'G', 'T')


convert_to_intlist(line) = map(x -> parse(Int, x), split(line))


calc_hamming_distance(s1, s2) = count(((c1, c2),) -> c1 != c2, zip(s1, s2))


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


function does_k_mer_occurs_approximately_in_text(text, kmer, k, d)
    for ix in 1:(length(text)-k+1)
        substring = text[ix:ix+k-1]
        if calc_hamming_distance(substring, kmer) <= d
            return true
        end
    end
    false
end


function motif_enumeration(texts, k, d)
    all_texts_approximately_contain_pattern(kmer) =
        all(does_k_mer_occurs_approximately_in_text(text, kmer, k, d) for text in texts)

    motifs = Set{String}()
    for text in texts
        for ix in 1:(length(text)-k+1)
            pattern = text[ix:ix+k-1]
            neighbours = generate_neighbourhood(pattern, d)
            for neighbour in neighbours
                if all_texts_approximately_contain_pattern(neighbour)
                    push!(motifs, neighbour)
                end
            end
        end
    end
    motifs
end


function main()
    k, d = convert_to_intlist(readline())
    texts = readlines()
    result = motif_enumeration(texts, k, d)
    println(join(result, " "))
end


main()
