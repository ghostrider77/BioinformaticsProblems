# Find the Most Frequent Words in a String

function most_frequent_k_mers(text, k)
    counts = Dict{AbstractString, Int64}()
    for ix in 1:(length(text)-k+1)
        k_mer = text[ix:ix+k-1]
        count = get(counts, k_mer, 0)
        counts[k_mer] = count + 1
    end
    maxcount = maximum(values(counts))
    [k_mer for (k_mer, count) in counts if count == maxcount]
end


function main()
    text = readline()
    k = parse(Int64, readline())
    result = most_frequent_k_mers(text, k)
    println(join(result, " "))
end


main()
