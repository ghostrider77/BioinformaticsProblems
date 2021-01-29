# Generate the d-Neighborhood of a String

const NUCLEOTIDES = ['A', 'C', 'G', 'T']


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


function main()
    pattern = readline()
    d = parse(Int64, readline())
    result = generate_neighbourhood(pattern, d)
    foreach(println, result)
end


main()
