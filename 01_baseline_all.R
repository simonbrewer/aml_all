set.seed(42)
library(tidyverse)
library(mlr3verse)

## ----------------------------------------------------------------------------
## Threshold functions
acc_threshold = function(p, th) {
  p$set_threshold(th)
  list(confusion = p$confusion, 
       costs = p$score(measures = msr("classif.acc"), 
                       task = task))
}

bacc_threshold = function(p, th) {
  p$set_threshold(th)
  list(confusion = p$confusion, 
       costs = p$score(measures = msr("classif.bacc"), 
                       task = task))
}

sens_threshold = function(p, th) {
  p$set_threshold(th)
  list(confusion = p$confusion, 
       costs = p$score(measures = msr("classif.sensitivity"), 
                       task = task))
}

spec_threshold = function(p, th) {
  p$set_threshold(th)
  list(confusion = p$confusion, 
       costs = p$score(measures = msr("classif.specificity"), 
                       task = task))
}

## ----------------------------------------------------------------------------
## Read in data
dat <- read.csv("./data/all.all.df.csv")

dat2 <- dat %>%
  select(rbin, txage, hla, tbi, abd, ci, mtx, mmf, agvhd, cgvhd,
         bmc_cdw, bmc_cd3, bmc_cd15, bmc_cd34, 
         pbc_cdw, pbc_cd3, pbc_cd15, pbc_cd34)

dat2 <- dat2 %>% 
  mutate_if(is.character, as.factor)  %>% 
  mutate_if(is.integer, as.numeric) %>%
  mutate(abd = tolower(abd)) %>%
  drop_na() %>%
  droplevels()

## Get $p(relapse)$ for baseline model
prbin = sum(as.numeric(dat2$rbin)-1) / nrow(dat2)

## Monte Carlo
## Monte Carlo loop

nbit = 1000

base_results <- data.frame(iteration = c(1:nbit),
                           acc = rep(NA, nbit),
                           auc = rep(NA, nbit),
                           sens = rep(NA, nbit),
                           spec = rep(NA, nbit)
)

for (i in 1:nbit) {
  if (i %% 100 == 0) {print(paste("Doing:", i))}
  
  df <- data.frame(id = seq(1:nrow(dat2)),
                   obs = as.numeric(dat2$rbin)-1,
                   pred = sample(as.numeric(dat2$rbin)-1))
  
  base_results$auc[i] <- PresenceAbsence::auc(df)$AUC[[1]]
  ## Get threshold
  opt_thresh <- 0.5
  opt_thresh <- optimal.thresholds(df, opt.methods = 3)[2]
  opt_cmx <- cmx(df, threshold = opt_thresh$pred)
  
  base_results$acc[i] <- (opt_cmx[1,1] + opt_cmx[2,2]) / nrow(df)
  # sensitivity(opt_cmx)
  base_results$sens[i] <- PresenceAbsence::sensitivity(opt_cmx)$sensitivity[[1]]
  base_results$spec[i] <- PresenceAbsence::specificity(opt_cmx)$specificity[[1]]
  
}
apply(base_results[,2:5], 2, mean, na.rm = TRUE)

