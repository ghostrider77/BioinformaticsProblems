# Find a Profile-most Probable k-mer in a String

struct ProfileColumn
    A::Float64
    C::Float64
    G::Float64
    T::Float64
end


convert_to_floatlist(line) = map(x -> parse(Float64, x), split(line))


function read_profile_matrix(lines, k)
    matrix = Array{Float64, 2}(undef, 4, k)
    for (ix, line) in enumerate(lines)
        matrix[ix, :] = convert_to_floatlist(line)
    end
    [ProfileColumn(column...) for column in eachcol(matrix)]
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


function main()
    text = readline()
    k = parse(Int64, readline())
    lines = readlines()
    profile_matrix = read_profile_matrix(lines, k)
    result = profile_most_probable_kmer(text, profile_matrix, k)
    println(result)
end


main()
