install.packages("docxtractr")

library(docxtractr)

NHIS <- read_docx('src/mcraig4/2014-nhis-sas-codebook.docx')
docx_tbl_count(NHIS)
all_tables <- docx_extract_all_tbls(NHIS)
brthwght <- assign_colnames(all_tables[[2]], row = 0)

brthwght[2,2]

for (i in all_tables) {
    return(i[2,2])
}


myfun <- function(all_tables) {
    return(all_tables[2, 2, drop=FALSE])
}


lapply(all_tables, myfun)
