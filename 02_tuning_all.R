set.seed(42)
library(tidyverse)
library(mlr3verse)
library(mlr3tuning)
library(mlr3tuningspaces)
library(paradox)

## ----------------------------------------------------------------------------
## Read in data
dat <- read.csv("./data/all.all.df.csv")

dat2 <- dat %>%
  select(rbin, txage, hla, tbi, abd, ci, mtx, mmf, agvhd, cgvhd,
         bmc_cdw, bmc_cd3, bmc_cd15, bmc_cd34, 
         pbc_cdw, pbc_cd3, pbc_cd15, pbc_cd34)

dat2 <- dat2 %>% 
  mutate(abd = tolower(abd)) %>%
  # mutate_if(is.character, as.factor)  %>% 
  # mutate_if(is.integer, as.numeric) %>%
  mutate(rbin = as.factor(rbin)) %>%
  drop_na() %>%
  droplevels()

## Set up task
task_all <- TaskClassif$new(id = "all", backend = dat2, 
                              target = "rbin")

# task_drugs$col_roles$stratum <- "code"

# print keys and learners
as.data.table(mlr_tuning_spaces)

## -------------------------------------------------------------------------- ##
## glmnet tuning
# tune learner with default search space
instance_glmnet = tune(
  method = "grid_search",
  task = task_all,
  learner = lts(lrn("classif.glmnet")),
  resampling = rsmp ("cv", folds = 5),
  measure = msr("classif.bacc"),
  term_evals = 100
)

# best performing hyperparameter configuration
instance_glmnet$result

## -------------------------------------------------------------------------- ##
## RF tuning
# tune learner with default search space
instance_rf = tune(
  method = "grid_search",
  task = task_all,
  learner = lts(lrn("classif.ranger")),
  resampling = rsmp ("cv", folds = 5),
  measure = msr("classif.bacc"),
  term_evals = 100
)

# best performing hyperparameter configuration
instance_rf$result


