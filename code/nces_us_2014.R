#http://nces.ed.gov/ccd/pubschuniv.asp
#ccd <- read.table("sc121a_supp.txt", sep="\t", header=T, fill=T, stringsAsFactors = F)
#md <- ccd[ccd$FIPST==24,]

library(haven)
require(dplyr)
setwd('C:/Users/RRosso/Documents/rnces')

ccd <- read_sas("./raw/sch14pre.sas7bdat")

ccd$DenomFreeRedLunch <- 0
ccd$DenomFreeRedLunch <- ccd$MEMBER[ccd$TOTFRL>=0]
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

write.csv(schl, file="./output/us_nces_schl_2014.csv")

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

write.csv(cty, file="./output/nces_cty_2014.csv")

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

write.csv(state, file="./output/nces_state_2014.csv")

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

write.csv(us, file="./output/nces_us_2014.csv")

# append geos
# nces <- rbind(cty, state, us)

# write.csv(nces, file="./output/us_nces_2014.csv")