library(readxl)
library(WriteXLS)

sixth_grade_ffxys <- read_xlsx('src/mcraig4/data/sixth_grade_ffxys.xlsx')
older_ffxys <- read_xlsx('src/mcraig4/data/older_ffxys.xlsx')

# Get rid of the first NA row
#sixth_grade_ffxys <- sixth_grade_ffxys[-1, ]
older_ffxys<- older_ffxys[-1, ]

# See how many matches there are between the two dataframe
merged <- merge(older_ffxys, sixth_grade_ffxys,by = 'Variable name')

# Combine these two dataframes, then unique() to get rid of duplicating values
rbind <- rbind(older_ffxys, sixth_grade_ffxys)
all_data_df <- unique(rbind)
#write.csv(all_data_df, file = 'src/mcraig4/data/combined_data.csv')
