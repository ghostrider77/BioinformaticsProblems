# Implement GreedyMotifSearch
using DataStructures

const NUCLEOTIDES = ('A', 'C', 'G', 'T')


struct ProfileColumn
    A::Float64
    C::Float64
    G::Float64
    T::Float64
end


function Base.argmax(column::ProfileColumn)
    ((getfield(column, symbol), String(symbol)) for symbol in fieldnames(ProfileColumn)) |> maximum |> last
end


convert_to_intlist(line) = map(x -> parse(Int, x), split(line))


calc_hamming_distance(s1, s2) = count(((c1, c2),) -> c1 != c2, zip(s1, s2))


function create_profile_matrix_from_motifs(motifs)
    nr_motifs = length(motifs)
    profile_matrix = ProfileColumn[]
    for column in zip(motifs...)
        counts = counter(column)
        vals = (get(counts, nucleotide, 0) / nr_motifs for nucleotide in NUCLEOTIDES)
        push!(profile_matrix, ProfileColumn(vals...))
    end
    profile_matrix
end


function calc_profile_matrix_score(motifs)
    profile_matrix = create_profile_matrix_from_motifs(motifs)
    consensus = join(map(argmax, profile_matrix))
    score = 0
    foldl((score, motif) -> score + calc_hamming_distance(motif, consensus), motifs; init=0)
end


calc_kmer_probability(kmer, profile_matrix) =
    foldl((p, (nucleotide, column)) -> p * getfield(column, Symbol(nucleotide)), zip(kmer, profile_matrix); init=1.0)


function profile_most_probable_kmer(text, profile_matrix, k)
    max_probability = 0.0
    most_probable_kmer = text[1:k]
    for ix in 1:(length(text)-k+1)
        kmer = text[ix:ix+k-1]
        p = calc_kmer_probability(kmer, profile_matrix)
        if p > max_probability
            max_probability = p
            most_probable_kmer = kmer
        end
    end
    most_probable_kmer
end


function greedy_motif_search(texts, k)
    best_motifs = map(text -> text[1:k], texts)
    best_score = calc_profile_matrix_score(best_motifs)
    for ix in 1:(length(texts[1])-k+1)
        kmer = texts[1][ix:ix+k-1]
        motifs = [kmer]
        for text in texts[2:end]
            profile = create_profile_matrix_from_motifs(motifs)
            motif = profile_most_probable_kmer(text, profile, k)
            push!(motifs, motif)
        end
        score = calc_profile_matrix_score(motifs)
        if score < best_score
            best_score = score
            best_motifs = motifs
        end
    end
    best_motifs
end


function main()
    k, t = convert_to_intlist(readline())
    texts = map(_ -> readline(), 1:t)
    result = greedy_motif_search(texts, k)
    foreach(println, result)
end


main()
