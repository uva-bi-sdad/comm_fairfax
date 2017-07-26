#### Created by: benjs23
#### Date: 6/26/2017
#### Overview: This code scrapes information from psychologytoday.com. It pulls the address and type of professional from
####           every therapist listed on their website in Fairfax County.

library(rvest)
library(XML)
library(stringr)
library(qdapRegex)
library(matrixcalc)
library(dplyr)

rm(q)
rm(p)


#html_text()  <<-- maybe use this to get better results

i = 1 #initialize index i
count = 1 #set therapist count to 1 (there are 20 per page)
p = vector(mode = "character", length = 2000)
typeTherapist = vector(mode = "character")
q <- vector(mode = "character", length = (2000))
##### This loop collects the href  (the web address code that links to each therapists' psychology today profile page)
##### for all Fairfax County therapists
while (count < 1005 ) {
    listURL <- read_html(paste0("https://therapists.psychologytoday.com/rms/prof_results.php?sid=1498155929.7585_14656&county=Fairfax&state=VA&rec_next=",count))
    count <- count + 20

    p <- listURL %>%
      html_nodes(".result-actions")

   # typeTemp <- listURL %>% html_nodes(".result-suffix span:nth-child(1)")

    for (i in (count-20):(count-1)) {
      if(i%%20 == 0)
      {
      q[i] <- sub(".*?profid=(.*?)&.*", "\\1", p[(i%%20)+20])
      #typeTherapist[i] <- typeTemp[(i%%20)+20]
      }
      else
      {
      q[i] <- sub(".*?profid=(.*?)&.*", "\\1", p[(i%%20)])
      #typeTherapist[i] <- typeTemp[(i%%20)]
      }
    }
    Sys.sleep(runif(1, 0.2, 2.9))
}

q <- q[q != ""]


q<-scan("~/git/comm_fairfax/data/comm_fairfax/original/therapist_href.txt", character(), quote = "")
addressMaster = vector(mode = "character", length = length(q))

##### This loop navigates to every therapists profile page on psychologytoday.com (using the href) and compiles an array of
##### all of their address strings
for(j in 1:length(q))
{
finalAddressTemp  = ''
profileURL <- read_html(paste0("https://therapists.psychologytoday.com/rms/prof_detail.php?profid=", q[j],"&rec_next=1&ref=1&sid=1498155929.7585_14656&county=Fairfax&state=VA&tr=ResultsProfileBtn"))

address <- profileURL %>% html_node(".address-rank-1 div:nth-child(1)") %>% xml_contents() %>% as.character()
    k=0
    streetAddress = 0
    addressLocality = 0
    addressRegion = 0
    postalCode = 0
    extractedAddress <- vector(mode = "character", length = length(address))
    for(k in 1:length(address))
    {
        extractedAddress[k]<-str_extract(address[k], regex("(?<=>).*(?=<)", dotall=TRUE))

    }

    extractedAddress<-na.omit(extractedAddress)
    if (as.numeric(substring(extractedAddress[length(extractedAddress)], 2, 4)) == 703|301|571|202|240|540)
    {
      extractedAddress <- extractedAddress[1:(length(extractedAddress)-1)]
    }
    for(l in 1:length(extractedAddress))
        {
          extractedAddress[l] <- str_trim(extractedAddress[l])
          finalAddressTemp <- paste(finalAddressTemp, extractedAddress[l])
    }

    addressMaster[j] <- str_trim(finalAddressTemp)
    rm(finalAddressTemp)
    Sys.sleep(runif(1, 0.2, 7))
}

# Not sure why I have a copy of the prior loop, so I'm leaving it in just in case.
# count2=1
# for(j in 1:length(q))
# {
#   finalAddressTemp  = ''
#   profileURL <- read_html(paste0("https://therapists.psychologytoday.com/rms/prof_results.php?sid=1498155929.7585_14656&county=Fairfax&state=VA&rec_next=",count2))
#
#   typeTherapist <- profileURL %>% html_nodes(".result-suffix span:nth-child(1)") %>% html_text()
#
#   k=0
#   streetAddress = 0
#   addressLocality = 0
#   addressRegion = 0
#   postalCode = 0
#   extractedAddress <- vector(mode = "character", length = length(address))
#   for(k in 1:length(address))
#   {
#     extractedAddress[k]<-str_extract(address[k], regex("(?<=>).*(?=<)", dotall=TRUE))
#
#   }
#
#   extractedAddress<-na.omit(extractedAddress)
#   if (as.numeric(substring(extractedAddress[length(extractedAddress)], 2, 4)) == 703|301|571|202|240|540)
#   {
#     extractedAddress <- extractedAddress[1:(length(extractedAddress)-1)]
#   }
#   for(l in 1:length(extractedAddress))
#   {
#     extractedAddress[l] <- str_trim(extractedAddress[l])
#     finalAddressTemp <- paste(finalAddressTemp, extractedAddress[l])
#   }
#
#   addressMaster[j] <- str_trim(finalAddressTemp)
#   rm(finalAddressTemp)
#   Sys.sleep(runif(1, 0.2, 7))
# }


####This removes addresses that don't start with a number (if only the city name or zip code was returned from web scraping)
totalMissing = 0
hrefMissing = vector()
for(m in 1:length(addressMaster))
{
    if(!grepl("[0-9]",substring(addressMaster[m], 1, 1)) )
    {
        totalMissing = totalMissing + 1
        hrefMissing[m] = q[m]

    }
}
hrefMissing <-na.omit(hrefMissing)



### Creates and exports the address combined with href data.
address_and_href <- as.data.frame(cbind(addressMaster, q))
colnames(address_and_href) <- c('address', 'href')
write.table(address_and_href,"~/git/comm_fairfax/data/comm_fairfax/original/masterAddress_and_href.txt")
write(addressMaster, "~/git/comm_fairfax/data/comm_fairfax/original/masterAddress.txt")
addressMasterImport <- read.table("~/git/comm_fairfax/data/comm_fairfax/original/masterAddress_and_href.txt")




##### The next section of code pulls the client focus and licenses held (type of therapist) of all the therapists, psychologists and psychiatrists.

q<-scan("~/git/comm_fairfax/data/comm_fairfax/original/therapist_href.txt", character(), quote = "")
typeTherapistMaster = vector(mode = "character", length = length(q))
for(j in 1:length(q))
{

  profileURL <- read_html(paste0("https://therapists.psychologytoday.com/rms/prof_detail.php?profid=", q[j],"&rec_next=1&ref=1&sid=1498155929.7585_14656&county=Fairfax&state=VA&tr=ResultsProfileBtn"))

  typeTherapist <- profileURL %>% html_node(".profile-title h2 , .nowrap span") %>% html_text()
  typeTherapist <- gsub(" ", "", str_replace_all(typeTherapist, "[\r\n]" , ""), fixed = TRUE)

  typeTherapistMaster[j] <- typeTherapist

  Sys.sleep(runif(1, 0.2, 3))
  print(j)
}
save(typeTherapistMaster ,file = "~/git/comm_fairfax/data/comm_fairfax/working/typeTherapist.RData")

##Client Focus of Therapist
q<-scan("~/git/comm_fairfax/data/comm_fairfax/original/therapist_href.txt", character(), quote = "")
clientFocusMaster = vector(mode = "character", length = length(q))
for(j in 1:length(q))
{

  profileURL <- read_html(paste0("https://therapists.psychologytoday.com/rms/prof_detail.php?profid=", q[j],"&rec_next=1&ref=1&sid=1498155929.7585_14656&county=Fairfax&state=VA&tr=ResultsProfileBtn"))

  clientFocus <- profileURL %>% html_node(".col-lg-5 .spec-list:nth-child(5)") %>% html_text()
  clientFocus <- gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", gsub(" ", "", str_replace_all(clientFocus, "[\r\n]" , ""), fixed = TRUE))

  print(clientFocus)
  clientFocusMaster[j] <- clientFocus
  print(j)
  Sys.sleep(runif(1, 0.2, 3))
}


save(clientFocusMaster ,file = "~/git/comm_fairfax/data/comm_fairfax/working/clientFocus.RData")

add<-as.data.frame(scan(file = "~/git/comm_fairfax/data/comm_fairfax/original/masterAddress_and_href.txt", what= character(), sep="\n"))

#href <- (rio::import(file ="~/git/comm_fairfax/data/comm_fairfax/original/masterAddress_and_href.txt" ))
#href <- sapply(href, function(x) href[x+1,])


###
colnames(add) <- "Address"
masterTherapistDF<- as.data.frame(cbind(add, typeTherapistMaster, clientFocusMaster))
colnames(masterTherapistDF) <- c("Address", "Type", "Client Focus")
save(masterTherapistDF2, file = "~/git/comm_fairfax/data/comm_fairfax/working/fairfaxTherapistMaster.RData")

load("~/git/comm_fairfax/data/comm_fairfax/working/latLongTherapist.RData")
View(latLongPlotData)

masterTherapistDF2 <- inner_join(masterTherapistDF, latLongPlotData, by="Address")
