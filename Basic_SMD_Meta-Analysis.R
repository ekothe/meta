## Specify the file containing your data
filename <- "data.csv"

### Specify the columns containing the Study ID and means, SDs, and Ns for intervention and control within the file

col.int.means     <- "Intervention.Means"
col.int.sds       <- "Intervention.SD"
col.int.ns        <- "Intervention.N"
col.cont.means    <- "Control.Means"
col.cont.sds      <- "Control.SD"
col.cont.ns       <- "Control.N"
col.study.id      <- "Study.ID"


### Specify the effect size measure

## Options in this template are SMD (standarised mean differences) and MD (raw mean differences). 
## In most cases SMD is most appropriate.

measure <- "SMD"

### Specify the model type. 
## In most cases REML should be the default

## Options in this template are 

# method <- "FE" # Fixed effect meta-analysis
# method <- "REML" # Default random effects meta-analysis
# method <- "DL" #  DerSimonian-Laird estimator
# method <- "HE" # Hedges estimator
# method <- "HS" # Hunter-Schmidt estimator
# method <- "SJ" # Sidik-Jonkman estimator
# method <- "ML" # maximum-likelihood estimator
# method <- "REML" # restricted maximum-likelihood estimator
# method <- "EB" # empirical Bayes estimator
# method <- "PM" # Paule-Mandel estimator
# method <- "GENQ" # generalized Q-statistic estimator

method <- "REML"


## ----setup, include=FALSE------------------------------------------------
library(metafor)
library(DT)
library(knitr)
library(RCurl)
library(dplyr)
library(forestmodel)
library(rmarkdown)

## These lines of code download and run the metafor_tidiers functions that implement broom type tidy data functions for rma objects
eval(parse(text = (getURL("https://raw.githubusercontent.com/talgalili/broom/master/R/metafor_tidiers.R", ssl.verifypeer = FALSE))))




## ----read_data-----------------------------------------------------------
dat <- read.csv(filename, stringsAsFactors = FALSE)

## ----calculate_ES--------------------------------------------------------
  dat_ES <-
    escalc(
    measure = measure,
    m1i = get(col.int.means),
    sd1i = get(col.int.sds),  
    n1i = get(col.int.ns),  
    m2i = get(col.cont.means),
    sd2i = get(col.cont.sds),  
    n2i = get(col.cont.ns),  
    data = dat
    )

## ----dat_es--------------------------------------------------------------
dat_ES


## ----run_MA--------------------------------------------------------------
dat_MA <- rma(yi, vi, data = dat_ES, slab = get(col.study.id), method=method)
dat_MA

## ----convenience, echo=FALSE---------------------------------------------
##These are some convenience functions that help put things into tables for easier interpretation.

model <- tidy.rma(dat_MA)

het.small <- glance.rma(dat_MA) %>% 
  select(one_of(c("k", "tau2", "se.tau2", "QE", "QEp", "I2")))


## ----summary_table, echo=FALSE-------------------------------------------
model
## ----het_table, eval=dat_MA$method!="FE", echo=FALSE---------------------
het.small
## ----forest, warning = FALSE, fig.height = (het.small$k*0.5)-------------
forest_rma(dat_MA, 
           study_labels = attr(dat_MA$yi, "slab"),
           format_options = list(colour  = "black", shape = 15, text_size = 4, banded = TRUE))

## ----funnel--------------------------------------------------------------
funnel(dat_MA, back="white")
