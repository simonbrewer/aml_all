library(tidyverse)
library(reshape2)
library(ggpubr)
library(lubridate)

myfun = function(x) {
  # as.numeric(x[1]) / as.numeric(x[2])
  as.numeric(x[2]) - as.numeric(x[1])
}

aml_file = "./data/aml.csv"

dat = read.csv(aml_file)
dat <- dat %>%
  filter(R != "")
dat$R <- tolower(dat$R)

names(dat)

## Add ID column
dat$ID = as.factor(seq(1:nrow(dat)))

## Fix l/I problem
dat$G.HLA.GVHD.prophy <- gsub("l", "I", dat$G.HLA.GVHD.prophy)

## Extract dates
bmc.df = dat[,c("ID", names(dat[,grep(glob2rx("D*MC"), names(dat))]))]

# bmc.df <- bmc.df %>%
#   filter(!is.na(D1MC) | !is.na(D2MC) | !is.na(D3MC) |
#            !is.na(D4MC) | !is.na(D5MC))

## Extract 'constants'
bmc.df$dot = dat$DOT
bmc.df$dor = dat$DOR
# bmc.df$dor[bmc.df$dor == ""] <- "2021-12-01"
bmc.df$dor[bmc.df$dor == ""] <- "12/1/21"
bmc.df$txage = dat$TXAGE
bmc.df$relapse = dat$R
bmc.df$sex = dat$G
bmc.df$rstatprtx = dat$RSTATPRTX
bmc.df$tbi = dat$TBI
bmc.df$ghgp <- dat$G.HLA.GVHD.prophy
bmc.df$agvhd = tolower(dat$aGVHD)

## Melt to long format
bmc.df = melt(bmc.df, id.vars = c("ID", "dot", "dor", "txage", "relapse",
                                  "sex", "rstatprtx", "ghgp", "tbi",
                                  "agvhd"), 
              variable.name = "test", value.name = "bdate")
bmc.df$bdate = mdy(bmc.df$bdate)
bmc.df$dot = mdy(bmc.df$dot)
bmc.df$dor = mdy(bmc.df$dor)
bmc.df$btime = bmc.df$bdate - bmc.df$dot
bmc.df$rtime = bmc.df$dor - bmc.df$dot

## Binary relapse variable for modeling
bmc.df$rbin = factor(bmc.df$relapse)

## Get chimerism data
bmc_cdw = dat[,c("ID", names(dat[,grep(glob2rx("*MCW"), names(dat))]))]
bmc_cd3 = dat[,c("ID", names(dat[,grep(glob2rx("*MC3"), names(dat))]))]
bmc_cd3 = bmc_cd3[, -ncol(bmc_cd3)]
bmc_cd15 = dat[,c("ID", names(dat[,grep(glob2rx("*MC15"), names(dat))]))]
bmc_cd34 = dat[,c("ID", names(dat[,grep(glob2rx("*MC34"), names(dat))]))]

## Melting
bmc_cdw = melt(bmc_cdw, id.vars = "ID", variable.name = "test", value.name = "prop")
bmc_cd3 = melt(bmc_cd3, id.vars = "ID", variable.name = "test", value.name = "prop")
bmc_cd15 = melt(bmc_cd15, id.vars = "ID", variable.name = "test", value.name = "prop")
bmc_cd34 = melt(bmc_cd34, id.vars = "ID", variable.name = "test", value.name = "prop")

bmc.df$cdw = bmc_cdw$prop
bmc.df$cd3 = bmc_cd3$prop
bmc.df$cd15 = bmc_cd15$prop
bmc.df$cd34 = bmc_cd34$prop

write.csv(bmc.df, "./data/aml.bmc.df.csv", row.names = FALSE)

## Peripheral blood

## Extract dates
pbc.df = dat[,c("ID", names(dat[,grep(glob2rx("D*PBC"), names(dat))]))]

pbc.df <- pbc.df %>%
  filter(!is.na(D1PBC) | !is.na(D2PBC) | !is.na(D3PBC) |
           !is.na(D4PBC) | !is.na(D5PBC))

## Extract 'constants'
pbc.df$dot = dat$DOT
pbc.df$dor = dat$DOR
# pbc.df$dor[pbc.df$dor == ""] <- "2021-12-01"
pbc.df$dor[pbc.df$dor == ""] <- "12/1/21"
pbc.df$txage = dat$TXAGE
pbc.df$relapse = dat$R
pbc.df$sex = dat$G
pbc.df$rstatprtx = dat$RSTATPRTX
pbc.df$tbi = dat$TBI
pbc.df$ghgp <- dat$G.HLA.GVHD.prophy
pbc.df$agvhd = tolower(dat$aGVHD)

## Melt to long format
pbc.df = melt(pbc.df, id.vars = c("ID", "dot", "dor", "txage", "relapse",
                                  "sex", "rstatprtx", "ghgp", "tbi",
                                  "agvhd"), 
              variable.name = "test", value.name = "pdate")
pbc.df$pdate = mdy(pbc.df$pdate)
pbc.df$dot = mdy(pbc.df$dot)
pbc.df$dor = mdy(pbc.df$dor)
pbc.df$ptime = pbc.df$pdate - pbc.df$dot
pbc.df$rtime = pbc.df$dor - pbc.df$dot

## Binary relapse variable for modeling
pbc.df$rbin = factor(pbc.df$relapse)

## Get chimerism data
pbc_cdw = dat[,c("ID", names(dat[,grep(glob2rx("*PBCW"), names(dat))]))]
pbc_cd3 = dat[,c("ID", names(dat[,grep(glob2rx("*PBC3"), names(dat))]))]
pbc_cd3 = pbc_cd3[, -ncol(pbc_cd3)]
pbc_cd15 = dat[,c("ID", names(dat[,grep(glob2rx("*PBC15"), names(dat))]))]
pbc_cd34 = dat[,c("ID", names(dat[,grep(glob2rx("*PBC34"), names(dat))]))]

## Melting
pbc_cdw = melt(pbc_cdw, id.vars = "ID", variable.name = "test", value.name = "prop")
pbc_cd3 = melt(pbc_cd3, id.vars = "ID", variable.name = "test", value.name = "prop")
pbc_cd15 = melt(pbc_cd15, id.vars = "ID", variable.name = "test", value.name = "prop")
pbc_cd34 = melt(pbc_cd34, id.vars = "ID", variable.name = "test", value.name = "prop")

pbc.df$cdw = pbc_cdw$prop
pbc.df$cd3 = pbc_cd3$prop
pbc.df$cd15 = pbc_cd15$prop
pbc.df$cd34 = pbc_cd34$prop

write.csv(pbc.df, "./data/aml.pbc.df.csv", row.names = FALSE)

all.df = bmc.df
all.df$bmc_cdw = all.df$cdw
all.df$bmc_cd3 = all.df$cd3
all.df$bmc_cd15 = all.df$cd15
all.df$bmc_cd34 = all.df$cd34
all.df = all.df %>% 
  select(-cdw, -cd3, -cd15, -cd34)

all.df$pdate = pbc.df$pdate
all.df$ptime = pbc.df$ptime
all.df$pbc_cdw = pbc.df$cdw
all.df$pbc_cd3 = pbc.df$cd3
all.df$pbc_cd15 = pbc.df$cd15
all.df$pbc_cd34 = pbc.df$cd34

write.csv(all.df, "./data/aml.all.df.csv", row.names = FALSE)

