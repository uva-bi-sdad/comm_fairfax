library(maps)
library(ggplot2)


# Data on shared state borders, used for the border share measure of similarity
borders = read.csv("./data/comm_fairfax/original/stateBorderLengths.csv")
pabb = unique(state.fips$abb)
border.share = matrix(0, 49, 49)
colnames(border.share) = rownames(border.share) = pabb

for(i in 1:49){
    # Current abbreviation
    abb = pabb[i]

    # Get border lengths
    shares = borders$LENGTH[grep(abb, borders$ST1ST2)]


    # Extract the shate abbreviations for matrix indexing
    current.border = grep(abb, borders$ST1ST2, value = T)
    current.border = sub(abb, "", current.border)
    current.border = sub("-", "", current.border)

    # But the share proportions in the right places in the border share matrix
    border.share[i, which(pabb %in% current.border)] = shares
}
