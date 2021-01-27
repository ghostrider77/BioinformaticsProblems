# Implement NumberToPattern

const NUCLEOTIDES = ('A', 'C', 'G', 'T')


function number_to_pattern(encoding, k)
    pattern = Char[]
    for _ in 1:k
        encoding, remainder = divrem(encoding, 4)
        push!(pattern, NUCLEOTIDES[remainder+1])
    end
    join(reverse(pattern))
end


function main()
    encoding = parse(Int64, readline())
    k = parse(Int64, readline())
    result = number_to_pattern(encoding, k)
    println(result)
end


main()
