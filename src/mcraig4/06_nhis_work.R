#install.packages("docxtractr")
library(docxtractr)
library(stringr)

NHIS <- read_docx('src/mcraig4/2014-nhis-sas-codebook.docx')
docx_tbl_count(NHIS)
all_tables <- docx_extract_all_tbls(NHIS)
brthwght <- assign_colnames(all_tables[[2]], row = 0)

brthwght[2,2]

for (i in all_tables) {
    return(i[1,2])
}


myfun <- function(all_tables) {
    return(all_tables[1, 2, drop=FALSE])
}

# It turns out the problem is that one of the 'tables' is just a character vector
sapply(all_tables, class)
# If we just remove that one from the record then it all works fine

# This is a slightly cleaner way to do what myfun does
descr = data.frame(var_descriptions = sapply(all_tables[-100], "[", 1, 2))
#write.csv(descr, file = 'output/mcraig4/nhis_descriptions.csv')

# Some regular expression (aka regex) stuff
# https://stackoverflow.com/questions/26809847/extract-text-between-certain-symbols-using-regular-expression-in-r
# https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
# This stuff is nuts! Here's an example of what we can do with it

char = brthwght[10, 1]
char

# Behold the beauty
regmatches(char, regexpr("(?<=\\/\\*)(.*)(?=\\*\\/)", char, perl = T))

# There's a lot going on here. The first link is to the stack overflow that got me this information. I googled "find characters between regex in r". The second link is to a cheat sheet for R's regex capabilities.
# The text in quotes in the regexpr function is the actual regular expression itself. (?<=_) looks ahead of '_' for stuff, (?=_) looks behind '_' for stuff, and (.*) means any text at all. The double slash \\ is an escape character you have to use if you want to use regex's 'special characters' as if they were text (so since / and * mean something programmatically, we need //\//*). So in words this says "find anything (.*) that comes after /* (?<=\\/\\*) and before */ (?=\\*\\/)
# I'll leave it to you to loop through. You'll need to consider that these tables could have different lengths. Also a good chance it'll break on some special case, so we can troublshoot if that happens.


variable <- str_extract_all(all_tables, "(?<=\\/\\*)(.*)(?=\\*\\/)")
require(reshape2)
variable$id <- rownames(variable)
variable_df <- melt(variable)

#write.csv(variable_df, 'output/mcraig4/nhis_variables_used.csv')
