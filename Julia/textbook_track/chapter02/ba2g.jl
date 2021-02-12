# Implement GibbsSampler
using DataStructures
using Random
using StatsBase

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


function create_profile_matrix_from_motifs_using_pseudocounts(motifs)
    nr_motifs = length(motifs)
    profile_matrix = ProfileColumn[]
    for column in zip(motifs...)
        counts = counter(column)
        vals = ((get(counts, nucleotide, 0) + 1) / (nr_motifs + 4) for nucleotide in NUCLEOTIDES)
        push!(profile_matrix, ProfileColumn(vals...))
    end
    profile_matrix
end


function calc_profile_matrix_score(motifs)
    profile_matrix = create_profile_matrix_from_motifs_using_pseudocounts(motifs)
    consensus = join(map(argmax, profile_matrix))
    foldl((score, motif) -> score + calc_hamming_distance(motif, consensus), motifs; init=0)
end


calc_kmer_probability(kmer, profile_matrix) =
    foldl((p, (nucleotide, column)) -> p * getfield(column, Symbol(nucleotide)), zip(kmer, profile_matrix); init=1.0)


function profile_randomly_generated_k_mer_in_text(text, profile_matrix, k)
    weights = Float64[]
    for ix in 1:(length(text)-k+1)
        kmer = text[ix:ix+k-1]
        p = calc_kmer_probability(kmer, profile_matrix)
        push!(weights, p)
    end
    index = sample(1:length(weights), ProbabilityWeights(weights))
    text[index:index+k-1]
end


function select_random_motifs(texts, k)
    motifs = String[]
    for text in texts
        ix = rand(1:(length(text)-k+1))
        motif = text[ix:ix+k-1]
        push!(motifs, motif)
    end
    motifs
end


remove_selected_row(motifs, index) = [motif for (ix, motif) in enumerate(motifs) if ix != index]


function gibbs_sampling(texts, k, t, n)
    motifs = select_random_motifs(texts, k)
    best_motifs = copy(motifs)
    best_score = calc_profile_matrix_score(motifs)
    for _ in 1:n
        ix = rand(1:t)
        reduced_motifs = remove_selected_row(motifs, ix)
        profile = create_profile_matrix_from_motifs_using_pseudocounts(reduced_motifs)
        motifs[ix] = profile_randomly_generated_k_mer_in_text(texts[ix], profile, k)
        score = calc_profile_matrix_score(motifs)
        if score < best_score
            best_score = score
            best_motifs = copy(motifs)
        end
    end
    (best_motifs, best_score)
end


function run_gibbs_sampling(texts, k, t, n; nr_iterations)
    Random.seed!(2112)
    best_motifs = nothing
    best_score = length(texts) * k
    for _ in 1:nr_iterations
        motifs, score = gibbs_sampling(texts, k, t, n)
        if score < best_score
            best_score = score
            best_motifs = motifs
        end
    end
    best_motifs
end


function main()
    k, t, n = convert_to_intlist(readline())
    texts = map(_ -> readline(), 1:t)
    result = run_gibbs_sampling(texts, k, t, n; nr_iterations = 20)
    foreach(println, result)
end


main()
