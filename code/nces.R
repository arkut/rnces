#http://nces.ed.gov/ccd/pubschuniv.asp
#ccd <- read.table("sc121a_supp.txt", sep="\t", header=T, fill=T, stringsAsFactors = F)
#md <- ccd[ccd$FIPST==24,]

library(haven)
require(dplyr)
#setwd('C:/Users/rrosso/Documents/data')
setwd('/Users/zoe/Dropbox/rclass')

ccd2 <- read_sas("sc131a_supp.sas7bdat")
ccd2_pregeo <- ccd2[,c("NCESSCH","MSTREE","MCITY","MSTATE","MZIP","MZIP4","LATCOD","LONCOD")]
write.csv(ccd2_pregeo, file="us_nces_pregeo_2013.csv")

balt <- ccd2[ccd2$FIPST==24 & ccd2$CONUM==24510,]
md <- ccd2[ccd2$FIPST==24,]
va <- ccd2[ccd2$FIPST==51,]
nova <- ccd2[ccd2$FIPST==51 & (ccd2$CONUM==51510 | ccd2$CONUM==51013 | ccd2$CONUM==51059),]

write.csv(nova, file="nova_nces_2013.csv")
novageo <- nova[,c("SCHNAM","NCESSCH","MSTREE","MCITY","MSTATE","MZIP","MZIP4","LATCOD","LONCOD","TOTFRL","MEMBER","FRELCH","REDLCH")]
write.csv(novageo, file="nova_nces_geo_2013.csv", row.names=TRUE)
#novageo <- read.csv("nova_nces_geo.csv", header=T, fill=T)
#novageo2 <- novageo[1:279,]

novageo$TOTFRL[novageo$TOTFRL<0] <- NA
novageo$MEMBER[novageo$MEMBER<0] <- NA
novageo$DenomFreeRedLunch <- novageo$MEMBER[novageo$TOTFRL>=0]
summary(novageo$DenomFreeRedLunch)
novageo$FRELCH[novageo$FRELCH < 0] <- NA
novageo$REDLCH[novageo$REDLCH < 0] <- NA

novageo$FREPCT    <- (novageo$FRELCH / novageo$DenomFreeRedLunch) * 100
novageo$REDPCT    <- (novageo$REDLCH / novageo$DenomFreeRedLunch) * 100
novageo$TOTFRLPCT <- (novageo$TOTFRL / novageo$DenomFreeRedLunch) * 100

# get county sums and pcts
novageo$totStu <- sum(novageo$MEMBER, na.rm=T)
novageo$cityDenomFreeRedLunch <- sum(novageo$DenomFreeRedLunch, na.rm=T)
novageo$totFree <- sum(novageo$FRELCH, na.rm=T)
novageo$totRed <- sum(novageo$REDLCH, na.rm=T)
novageo$totFRP <- sum(novageo$TOTFRL, na.rm=T)
novageo$totPctFree <- (novageo$totFree / novageo$cityDenomFreeRedLunch) * 100
novageo$totPctRed  <- (novageo$totRed / novageo$cityDenomFreeRedLunch) * 100
novageo$totPctFRP <- (novageo$totFRP / novageo$cityDenomFreeRedLunch) * 100

write.csv(novageo, file="nova_nces_geo_2013.csv")

#total students: MEMBER
#free lunch: FRELCH
#reduced-price lunch: REDLCH
md$TOTFRL[md$TOTFRL<0] <- NA
md$DenomFreeRedLunch <- md$TOTFRL

balt$DenomFreeRedLunch[balt$TOTFRL < 0] <- NA

summary(md$DenomFreeRedLunch)
summary(balt$DenomFreeRedLunch)
summary(balt$MEMBER)

balt$MEMBER[balt$MEMBER < 0] <- NA
balt$FRELCH[balt$FRELCH < 0] <- NA
balt$REDLCH[balt$REDLCH < 0] <- NA
balt$TOTFRL[balt$TOTFRL < 0] <- NA


md$MEMBER[md$MEMBER < 0] <- NA
md$FRELCH[md$FRELCH < 0] <- NA
md$REDLCH[md$REDLCH < 0] <- NA
md$TOTFRL[md$TOTFRL < 0] <- NA

balt$FREPCT <- (balt$FRELCH / balt$DenomFreeRedLunch) * 100
balt$REDPCT <- (balt$REDLCH / balt$DenomFreeRedLunch) * 100
balt$TOTFRLPCT <- (balt$TOTFRL / balt$DenomFreeRedLunch) * 100

#write.csv(balt, file="baltimore_nces.csv")
baltgeo <- read.csv("baltimore_nces_geo.csv", header=T, fill=T)
baltgeo2 <- baltgeo[1:197,]

baltAll <- merge(balt, baltgeo2, by="NCESSCH")

baltAll$totStu <- sum(baltAll$MEMBER, na.rm=T)
baltAll$cityDenomFreeRedLunch <- sum(baltAll$DenomFreeRedLunch, na.rm=T)
baltAll$totFree <- sum(baltAll$FRELCH, na.rm=T)
baltAll$totRed <- sum(baltAll$REDLCH, na.rm=T)
baltAll$totFRP <- sum(baltAll$TOTFRL, na.rm=T)
totPctFree <- (baltAll$totFrelch / baltAll$cityDenomFreeRedLunch)
totPctRed  <- (baltAll$totRedlch / baltAll$cityDenomFreeRedLunch)
totPctFRP <- (baltAll$totFRP / baltAll$cityDenomFreeRedLunch)

Sandtown <- baltAll[baltAll$CensusTract==1501.00 | baltAll$CensusTract==1502.00 | baltAll$CensusTract==1601.00 |
                            baltAll$CensusTract==1602.00 | baltAll$CensusTract==1603.00 | baltAll$CensusTract==1604.00, ]

Sandtown$totStu <- sum(Sandtown$MEMBER, na.rm=T)
Sandtown$totFrelch <- sum(Sandtown$FRELCH, na.rm=T)
Sandtown$totPctFree <- (Sandtown$totFrelch / Sandtown$totStu)

totTractSum <- baltAll %>%
        group_by(CensusTract) %>%
        summarise(tractTotStu = sum(MEMBER),
                  tractDenomFreeRedLunch = sum(DenomFreeRedLunch), 
                  tractFree = sum(FRELCH), 
                  tractRed = sum(REDLCH), 
                  tractFRL = sum(TOTFRL), 
                  tractFreePct = (sum(FRELCH) / tractDenomFreeRedLunch)*100,
                  tractRedPct = (sum(REDLCH) / tractDenomFreeRedLunch)*100,
                  tractFRLPct = (sum(TOTFRL) / tractDenomFreeRedLunch)*100,
                  tractCount = n())

totTractSum$geoID <- 1

baltSum <- merge(baltAll, totTractSum, by="CensusTract")

totCountySum <- baltAll %>%
        group_by(CONUM) %>%
        summarise(coTotStu = sum(MEMBER),
                  coDenomFreeRedLunch = sum(DenomFreeRedLunch), 
                  coFree = sum(FRELCH), 
                  coRed = sum(REDLCH), 
                  coFRL = sum(TOTFRL), 
                  coFreePct = (sum(FRELCH) / coDenomFreeRedLunch)*100,
                  coRedPct = (sum(REDLCH)  / coDenomFreeRedLunch)*100,
                  coFRLPct = (sum(TOTFRL)  / coDenomFreeRedLunch)*100,
                  coCount = n())

totCountySum$geoID <- 2

baltSum <- merge(baltSum, totCountySum, by="CONUM")

totMDCountySum <- md %>%
        group_by(CONUM) %>%
        summarise(coTotStu = sum(MEMBER),
                  coDenomFreeRedLunch = sum(DenomFreeRedLunch), 
                  coFree = sum(FRELCH), 
                  coRed = sum(REDLCH), 
                  coFRL = sum(TOTFRL), 
                  coFreePct = (sum(FRELCH) / coDenomFreeRedLunch)*100,
                  coRedPct = (sum(REDLCH)  / coDenomFreeRedLunch)*100,
                  coFRLPct = (sum(TOTFRL)  / coDenomFreeRedLunch)*100,
                  coCount = n())

totCountySum$geoID <- 2


#par(pch=22, col="red")
#par(mfrow=c(2,4))
#plot(baltSum$CensusTract, baltSum$tractFreePct, type="n")
#lines(baltSum$CensusTract, baltSum$tractFreePct, type="l")
nces_geosum <- function(data, geo) {
        geoSum <- data %>%
                group_by(data$geo) %>%
                summarise(geoTotStu = sum(MEMBER),
                          geoDenomFreeRedLunch = sum(DenomFreeRedLunch),
                          geoFree = sum(FRELCH), 
                          geoRed = sum(REDLCH), 
                          geoFRL = sum(TOTFRL), 
                          geoFreePct = (sum(FRELCH) / geoDenomFreeRedLunch)*100,
                          geoRedPct = (sum(REDLCH)  / geoDenomFreeRedLunch)*100,
                          geoFRLPct = (sum(TOTFRL)  / geoDenomFreeRedLunch)*100,
                          geoCount = n()
                          )
}

nces_geosum(nova, nova$CONUM)
