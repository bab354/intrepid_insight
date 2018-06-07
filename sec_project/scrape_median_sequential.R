setwd("C:/Users/jakek/Documents/intrepidinsight/scrape_median")
library('rvest')
library('dplyr')
library('tidyr')
library('stringr')
library('stringi')
library('XML')
library('htmltab')
library('edgar')
library('readr')
library('edgarWebR')
library('data.table')
library(htmltab)
library('quanteda')
library(qdapRegex)
library(parallel)
library('pushoverr')

## detect number of pages of results
for (i in seq(1,1000,1)) {
  dummytable<-data.table(header_search("DEF 14A", from=2018, page=i))
  
  if (length(dummytable$company_name)==0) break
  resultsnum<-i
}


latest_index<-lapply(1:resultsnum, function(x) data.table(header_search("DEF 14A", from=2018, page=x))[!is.na(filing_href)])
latest_index<-data.table(Reduce(rbind, latest_index))
latest_index<-latest_index[,html_url:=paste0(str_replace(filing_href,basename(filing_href),""),htmltab(doc=filing_href, which=1)[1,"Document"]), by=1:nrow(latest_index)]
latest_index<-latest_index[,fulltext:=str_replace_all(paste(parse_filing(html_url)$text, collapse=" "),"[\r\n]", " "),by=1:nrow(latest_index)][, fulltext:=rm_white(fulltext)]
latest_index<-latest_index[,median_context:=str_extract_all(stri_trans_general(fulltext, "latin-ascii"), "([^\\s]+\\s){0,20}(\\$(\\d|,)+\\d){1}")]
results<-latest_index[,median_context[[1]], by=list(company_name, filing_href, form, filing_date, html_url)][,truth:=str_detect(tolower(V1), "median")  & str_detect(tolower(V1), "employee"),list(html_url, V1)]
results<-results[truth==TRUE, list(form, filing_date, company_name, html_url, V1)][,median_comp:=str_extract(V1, "(\\$(\\d|,)+\\d)$")]
save(results, file="median_scraped.RData")
results$rand<-runif(nrow(results))
write.csv(results, file="allofit.csv")


if (TRUE==FALSE){

## method 2: using the recent filings function of edgarWebR - currently not active.
for (i in seq(1,1000,1)) {
print(paste("Doing:",i))
latest_index<-data.table(header_search("DEF 14A", from=2018, page=i))
if (length(latest_index$company_name)==0) break
latest_index<-latest_index[1:100][!is.na(filing_href)]
## generate the url to the file
latest_index[,html_url:=paste0(str_replace(filing_href,basename(filing_href),""),htmltab(doc=filing_href, which=1)[1,"Document"]), by=1:nrow(latest_index)]
## extract the text
latest_index<-latest_index[,fulltext:=str_replace_all(paste(parse_filing(html_url)$text, collapse=" "),"[\r\n]", " "),by=1:nrow(latest_index)][, fulltext:=rm_white(fulltext)]
##latest_index<-latest_index[,median_context:=str_extract_all(tolower(fulltext), "([^\\s]+\\s){0,20}median.?(\\s[^\\s]+){0,10}")]
latest_index<-latest_index[,median_context:=str_extract_all(stri_trans_general(fulltext, "latin-ascii"), "([^\\s]+\\s){0,20}(\\$(\\d|,)+\\d){1}")]
results<-latest_index[,median_context[[1]], by=list(company_name, filing_href, form, filing_date, html_url)][,fulltext:=NULL][,truth:=str_detect(tolower(V1), "median")  & str_detect(tolower(V1), "employee"),list(html_url, V1)]
results<-results[truth==TRUE, list(form, filing_date, company_name, html_url, V1)][,median_comp:=str_extract(V1, "(\\$(\\d|,)+\\d)$")]
if (i==1){
  master<-results
} else{
master<-rbind(results, master)
}

}

save(master, file="median_scraped.RData")

stopCluster(cl)
}

pushover(message = paste0("Median Scraper Finished Completely ", as.character(i)), user ="u3nyqi76hgi8t2e2j6yhf3x9hd4vxs" , app ="akx69yf9zq5jda1ujdw9eias9b41kp" )

## train classifier 
