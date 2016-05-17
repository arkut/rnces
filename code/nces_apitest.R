#http://nces.ed.gov/ccd/pubschuniv.asp
#ccd <- read.table("sc121a_supp.txt", sep="\t", header=T, fill=T, stringsAsFactors = F)
#md <- ccd[ccd$FIPST==24,]
#FCC geocode API website: https://www.fcc.gov/developers/census-block-conversions-api
#NCES file layout: https://nces.ed.gov/ccd/Data/txt/sc131alay.txt

library(haven)
require(dplyr)
library('RCurl')
library('XML')
library('httr')
#setwd('/Users/zoe/Dropbox/rclass')
setwd('C:/Users/rrosso/Dropbox/rclass')
url = "http://nces.ed.gov/ccd/Data/zip/sc131a_supp_sas.zip"

temp <- tempfile()
download.file(url, temp)
ccd2 <- read_sas(unzip(temp))
unlink(temp)

ccd2_pregeo <- ccd2[,c("NCESSCH","MSTREE","MCITY","MSTATE","MZIP","MZIP4","LATCOD","LONCOD")]
write.csv(ccd2_pregeo, file="us_nces_pregeo.csv")

geoTest = getURL("http://data.fcc.gov/api/block/2010/find?latitude=40.0&longitude=-85")

fcc = handle("http://data.fcc.gov/api/block/2010/find?")
fccTest <- GET("http://data.fcc.gov/api/block/2010/find", query = list(latitude = 40.0, longitude = -85))
#how to parse XML response (get JSON instead?)
#how to submit GET for each lat/long on file
#how to join response block/block group codes onto file