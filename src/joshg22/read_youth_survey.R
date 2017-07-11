# read in youth survey data and format it
#library(haven)
#youth_survey <- read_sas("~/mounts/lightfoot/sdal/projects/comm_fairfax/original/youth_survey/ys_2016_cluster.sas7bdat")
#write.csv(youth_survey, "~/mounts/lightfoot/sdal/projects/comm_fairfax/original/youth_survey/youth_survey_2016.csv",row.names = FALSE)

library(dplyr)

youth_survey <- read.csv("~/mounts/lightfoot/sdal/projects/comm_fairfax/original/youth_survey/youth_survey_2016.csv")

# read in the .sas code to format the data
data_dict <- read.table("~/Desktop/Fairfax Youth Survey/data_dict.txt",sep=";")
data_dict <- as.data.frame(data_dict[-c(1259),])

breaks <- c(0,which(data_dict[,1] == ""))

data_dict2 <- list()
for(i in 1:(length(breaks)-1)){
  data_dict2[[i]] <- paste( data_dict[(breaks[i]+2):(breaks[i+1]-1),] )
  thisname <- paste(data_dict[breaks[i]+1,])
  names(data_dict2)[i] <- substr(thisname,7,nchar(thisname)-4)
}

# remove trailing underscore if it exists
names <- names(data_dict2)
names_ind <- substr(names,nchar(names),nchar(names))=="_"
names[names_ind] <- substr(names[names_ind],1,nchar(names[names_ind])-1)
names(data_dict2) <- names

# get matching columns in youth_survey
names(youth_survey) %in% names(data_dict2)
# columns that don't match: CLUSTER_CODE, REGION_CODE, wgt
ind_match <- match(names(data_dict2),names(youth_survey))

# apply each list into two vectors by splitting strings at '='
data_dict3 <- lapply(data_dict2, function(x){
  as.data.frame( t(as.data.frame( strsplit(x, " = ") ) ) ) })
for(i in 1:length(data_dict3)){
  rownames(data_dict3[[i]]) <- NULL
  colnames(data_dict3[[i]]) <- c("value","desc")
  data_dict3[[i]][,1] <- as.numeric(data_dict3[[i]][,1])
}

# recode rows of youth_survey
youth_survey_desc <- youth_survey
for(i in 1:length(ind_match)){
  if(!is.na(ind_match[i])){
    youth_survey_desc[,ind_match[i]] <- data_dict3[[i]]$desc[ match( youth_survey_desc[,ind_match[i]], data_dict3[[i]]$value ) ]
  }
}

write.csv(youth_survey_desc,"~/mounts/lightfoot/sdal/projects/comm_fairfax/original/youth_survey/youth_survey_2016_formatted.csv",row.names = FALSE)


