# Generate the k-mer Composition of a String

function calc_k_mer_composition(text, k)
    composition = String[]
    for ix in 1:(length(text)-k+1)
        substring = text[ix:ix+k-1]
        push!(composition, substring)
    end
    sort(composition)
end


function main()
    k = parse(Int64, readline())
    text = readline()
    result = calc_k_mer_composition(text, k)
    foreach(println, result)
end


main()
