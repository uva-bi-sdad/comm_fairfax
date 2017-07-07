#### Created by: benjs23
#### Date: 6/26/2017
#### Overview: This code scrapes information from psychologytoday.com. It pulls the address and type of professional from
####           every therapist listed on their website in Fairfax County.

library(rvest)
library(XML)
library(stringr)
library(qdapRegex)

rm(q)
rm(p)


#html_text()  <<-- maybe use this to get better results
i = 1 #initialize index i 
count = 1 #set therapist count to 1 (there are 20 per page)
p = vector(mode = "character", length = 2000)
typeTherapist = vector(mode = "character")
q <- vector(mode = "character", length = (2000))
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
      typeTherapist[i] <- typeTemp[(i%%20)+20]
      }
      else
      {
      q[i] <- sub(".*?profid=(.*?)&.*", "\\1", p[(i%%20)])
      typeTherapist[i] <- typeTemp[(i%%20)]
      }
    }
    Sys.sleep(runif(1, 0.2, 2.9))
}  

q <- q[q != ""]


q<-scan("~/git/comm_fairfax/data/comm_fairfax/original/therapist_href.txt", character(), quote = "")
addressMaster = vector(mode = "character", length = length(q))

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


count2=1
for(j in 1:length(q))
{
  finalAddressTemp  = ''
  profileURL <- read_html(paste0("https://therapists.psychologytoday.com/rms/prof_results.php?sid=1498155929.7585_14656&county=Fairfax&state=VA&rec_next=",count2))
  
  typeTherapist <- profileURL %>% html_nodes(".result-suffix span:nth-child(1)") %>% html_text()
  
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




address_and_href <- as.data.frame(cbind(addressMaster, q))
colnames(address_and_href) <- c('address', 'href')
write.table(address_and_href,"~/git/comm_fairfax/data/comm_fairfax/original/masterAddress_and_href.txt")
write(addressMaster, "~/git/comm_fairfax/data/comm_fairfax/original/masterAddress.txt")
addressMasterImport <- read.table("~/git/comm_fairfax/data/comm_fairfax/original/masterAddress_and_href.txt")


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

