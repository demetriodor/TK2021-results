library(RCurl)
library(rvest)
library(stringr)
library(dplyr)
library(qdapRegex)

# get the list of municipalities
source ('./gemeentes.R')

all<-rm_between(all, '"', '"', extract=TRUE)[[1]]

# prepare the output table
tk2021 = data.frame(matrix (NA, nrow=length(all), ncol=17+1))
names(tk2021) = c('gemeente', "VVD","PVV","CDA","D66","GL","SP","PVDA","CU","PVDD","50PLUS","SGP","DENK","FVD","BIJ1","JA21","VOLT", "BBB") 

parties = names(tk2021)[-1]


for (i in 1:length(all)){ #for all municipalities
  url = paste0('https://www.verkiezingensite.nl/uitslag/', all[i]) # form the url of the website
  
  h<-read_html(url, encoding='UTF-8') # read the website
  
  # get the name of the gemeente
  gemeente = html_text(html_nodes(h,xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "block text-2xl uppercase font-bold tracking-normal", " " ))]'))
  
  # get the part with the results
  x.all = html_nodes(h,xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "flex flex-col md:flex-row gap-x-14", " " ))]//tbody//tr/td')
  
  # parse the results
  out<-NA
    for (j in 1:length(x.all)){
    x = html_nodes(h,xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "flex flex-col md:flex-row gap-x-14", " " ))]//tbody//tr/td')[j] %>%
      html_text()
    x = gsub("[\n,\t]","",x)
    x = str_trim(x)
    x = sub('\\s+', ', ', x)
    x = sub('\\,.*', '', x)
    out[j] <- x
  }
  
  # make the results into a table
  out.table = matrix(out, ncol=6, byrow=T)
  out.table = data.frame (out.table[,-4])
  names(out.table) = c('party', 'votes.21', 'share.21', 'votes.17', 'share.17')
  
  out.table <- out.table %>%
    mutate (votes.21 = as.numeric(str_replace_all(votes.21, "\\.", "")),
            votes.17 = as.numeric(str_replace_all(votes.17, "\\.", "")),
            share.21 = as.numeric(str_replace_all(share.21, "%", ""))/10,
            share.17 = as.numeric(str_replace_all(share.17, "%", ""))/10,
            change.share = share.21 - share.17
    )
  
  # assign the results to the output table
  tk2021$gemeente[i] <- tolower(gemeente)
  for (k in 1: length(parties)){
    tk2021[i,parties[k]] = out.table$change.share[out.table$party==parties[k]]
  }
}
