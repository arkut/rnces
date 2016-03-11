#http://nces.ed.gov/ccd/pubschuniv.asp
#ccd <- read.table("sc121a_supp.txt", sep="\t", header=T, fill=T, stringsAsFactors = F)
#md <- ccd[ccd$FIPST==24,]

library(haven)
require(dplyr)
#setwd('C:/Users/rrosso/Documents/data')
setwd('/Users/zoe/Dropbox/rclass')

ccd <- read_sas("sc131a_supp.sas7bdat")

ccd$DenomFreeRedLunch <- ccd$MEMBER[us$TOTFRL>=0]
schl <- ccd %>%
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

write.csv(schl, file="us_nces_schl_2013.csv")

cty <- schl %>%
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

cty$FIPST <- substr(cty$CONUM,1,2)
head(cty$FIPST)
cty$country <- "US"

write.csv(cty, file="nces_cty_2013.csv")

state <- schl %>%
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

state$CONUM <- NA
state$country <- "US"

write.csv(state, file="nces_state_2013.csv")

us <- schl %>%
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
us$CONUM <- NA
us$FIPST <- NA
us$country <- "US"

write.csv(us, file="nces_us_2013.csv")

# append geos
nces <- rbind(cty, state, us)

write.csv(nces, file="us_nces_2013.csv")