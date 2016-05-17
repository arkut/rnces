#http://nces.ed.gov/ccd/pubschuniv.asp
#ccd <- read.table("sc121a_supp.txt", sep="\t", header=T, fill=T, stringsAsFactors = F)
#md <- ccd[ccd$FIPST==24,]

library(haven)
require(dplyr)
setwd('C:/Users/RRosso/Documents/rnces')

ccd13 <- read_sas("./raw/sc131a_supp.sas7bdat")

ccd13$DenomFreeRedLunch <- 0
ccd13$DenomFreeRedLunch <- ccd13$MEMBER[ccd$TOTFRL>=0]
schl13 <- ccd13 %>%
  select(DenomFreeRedLunch,FIPST,CONUM,SCHNAM,NCESSCH,MSTREE,MCITY,MSTATE,MZIP,MZIP4,LATCOD,LONCOD,TOTFRL,MEMBER,FRELCH,REDLCH) %>%
  filter(
    FRELCH >= 0,
    REDLCH >= 0,
    TOTFRL >= 0,
    MEMBER >= 0
  ) %>%
  mutate(
    DenomFreeRedLunch = MEMBER[TOTFRL>=0],
    FrePct = (FRELCH / DenomFreeRedLunch) * 100,
    RedPct = (REDLCH / DenomFreeRedLunch) * 100,
    TotFRLPct = (TOTFRL / DenomFreeRedLunch) * 100
  )

write.csv(schl13, file="./output/us_nces_schl_2013.csv")

cty13 <- schl13 %>%
  group_by(CONUM) %>%
  summarise(
    FreLch = sum(FRELCH, na.rm = T),
    RedLch = sum(REDLCH, na.rm = T),
    TotFRL = sum(TOTFRL, na.rm = T),
    DenomFreeRedLunch = sum(DenomFreeRedLunch, na.rm = T)
  ) %>%
  mutate(
    FrePct = (FreLch / DenomFreeRedLunch) * 100,
    RedPct = (RedLch / DenomFreeRedLunch) * 100,
    TotFRLPct = (TotFRL / DenomFreeRedLunch) * 100
  )

cty13$FIPST <- substr(cty13$CONUM,1,2)
head(cty13$FIPST)
cty13$country <- "US"

write.csv(cty13, file="./output/nces_cty_2013.csv")

state13 <- schl13 %>%
  group_by(MSTATE) %>%
  summarise(
    FreLch = sum(FRELCH, na.rm = T),
    RedLch = sum(REDLCH, na.rm = T),
    TotFRL = sum(TOTFRL, na.rm = T),
    DenomFreeRedLunch = sum(DenomFreeRedLunch, na.rm = T)
  ) %>%
  mutate(
    FrePct = (FreLch / DenomFreeRedLunch) * 100,
    RedPct = (RedLch / DenomFreeRedLunch) * 100,
    TotFRLPct = (TotFRL / DenomFreeRedLunch) * 100
  )

state13$CONUM <- NA
state13$country <- "US"

write.csv(state13, file="./output/nces_state_2013.csv")

us13 <- schl13 %>%
  summarise(
    FreLch = sum(FRELCH, na.rm = T),
    RedLch = sum(REDLCH, na.rm = T),
    TotFRL = sum(TOTFRL, na.rm = T),
    DenomFreeRedLunch = sum(DenomFreeRedLunch, na.rm = T)
  ) %>%
  mutate(
    FrePct = (FreLch / DenomFreeRedLunch) * 100,
    RedPct = (RedLch / DenomFreeRedLunch) * 100,
    TotFRLPct = (TotFRL / DenomFreeRedLunch) * 100
  )
us13$CONUM <- NA
us13$FIPST <- NA
us13$country <- "US"

write.csv(us13, file="./output/nces_us_2013.csv")

# append geos
# nces <- rbind(cty, state, us)

# write.csv(nces, file="./output/us_nces_2013.csv")