#http://nces.ed.gov/ccd/pubschuniv.asp
#ccd <- read.table("sc121a_supp.txt", sep="\t", header=T, fill=T, stringsAsFactors = F)
#md <- ccd[ccd$FIPST==24,]

library(haven)
require(dplyr)
setwd('C:/Users/RRosso/Documents/rnces')

ccd12 <- read_sas("./raw/sc121a_supp.sas7bdat")

ccd12$DenomFreeRedLunch <- 0
ccd12$DenomFreeRedLunch <- ccd12$MEMBER[ccd12$TOTFRL>=0]
schl12 <- ccd12 %>%
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

write.csv(schl12, file="./output/us_nces_schl_2012.csv")

cty12 <- schl12 %>%
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

cty12$FIPST <- substr(cty12$CONUM,1,2)
head(cty12$FIPST)
cty12$country <- "US"

write.csv(cty12, file="./output/nces_cty_2012.csv")

state12 <- schl12 %>%
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

state12$CONUM <- NA
state12$country <- "US"

write.csv(state12, file="./output/nces_state_2012.csv")

us12 <- schl12 %>%
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
us12$CONUM <- NA
us12$FIPST <- NA
us12$country <- "US"

write.csv(us12, file="./output/nces_us_2012.csv")

# append geos
# nces <- rbind(cty, state, us)

# write.csv(nces, file="./output/us_nces_2012.csv")