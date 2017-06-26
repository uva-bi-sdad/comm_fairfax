library(rvest)
library(XML)
library(stringr)

library(qdapRegex)

rm(q)
rm(p)

i = 1
count = 1
p = vector(mode = "character", length = 2000)

q <- vector(mode = "character", length = (2000))
while (count < 25 ) {
    listURL <- read_html(paste0("https://therapists.psychologytoday.com/rms/prof_results.php?sid=1498155929.7585_14656&county=Fairfax&state=VA&rec_next=",count))
    count <- count + 20
    
    p <- listURL %>%
      html_nodes(".result-actions")
    

    for (i in (count-20):(count-1)) {
      if(i%%20 == 0)
      {
      q[i] <- sub(".*?profid=(.*?)&.*", "\\1", p[(i%%20)+20])
      }
      else
      {
      q[i] <- sub(".*?profid=(.*?)&.*", "\\1", p[(i%%20)])
      }
    }
   
}  

q <- q[q != ""]

for(j in 1:15)
{
  
profileURL <- read_html(paste0("https://therapists.psychologytoday.com/rms/prof_detail.php?profid=", q[7],"&rec_next=1&ref=1&sid=1498155929.7585_14656&county=Fairfax&state=VA&tr=ResultsProfileBtn"))

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
    
#tempAddress <- str_split(address, "\n")

#finalAddress <- append(finalAddress, tail(str_split((str_split(tempAddress[[1]][[2]], "<br>")[[1]]), "    ")[[1]], n = 1))
}
          