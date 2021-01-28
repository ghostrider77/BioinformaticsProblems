# Find a Position in a Genome Minimizing the Skew

function get_skewness_argmins(genome)
    skew = 0
    min_skew_value = 0
    min_skew_indices = [0]

    for (ix, nucleotide) in enumerate(genome)
        if nucleotide == 'C'
            skew -= 1
        elseif nucleotide == 'G'
            skew += 1
        end

        if skew == min_skew_value
            push!(min_skew_indices, ix)
        elseif skew < min_skew_value
            min_skew_value = skew
            min_skew_indices = [ix]
        end
    end
    min_skew_indices
end


function main()
    genome = readline()
    result = get_skewness_argmins(genome)
    println(join(result, " "))
end


main()
