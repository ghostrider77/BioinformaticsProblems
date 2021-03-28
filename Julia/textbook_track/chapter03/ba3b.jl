# Reconstruct a String from its Genome Path

function calc_string_spelled_by_genome_path(kmers)
    text = collect(kmers[1])
    for kmer in kmers[2:end]
        push!(text, kmer[end])
    end
    join(text)
end


function main()
    data = readlines()
    result = calc_string_spelled_by_genome_path(data)
    println(result)
end


main()
