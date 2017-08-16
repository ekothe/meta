## Specify the file containing your data
dat_comp <- read.csv("missing_data.csv")


### Specify the columns containing the means, SDs, and Ns for intervention and control within the file

col.int.means     <- "m1"
col.int.sds       <- "sd1"
col.int.ns        <- "n1"
col.cont.means    <- "m2"
col.cont.sds      <- "sd2"
col.cont.ns       <- "n2"

col.dval <- "dval"
col.tval <-"tval"
col.pval <- "pval"
col.sign <- "sign"

#This script demonstrates the method for assessembling SMDs described by Wolfgang Viechtbauer at http://metafor-project.org/doku.php/tips:assembling_data_smd. The comparable method for Odds Ratios is available here: http://metafor-project.org/doku.php/tips:assembling_data_or 

#If you are working with other data types or have different pieces of information about studies available you may find the package 'compute.es' helpful.

## Load packages
## ----setup------------------------------------------------
library(knitr)
library(metafor)

## Let's compile!
dat_comp
### Start by calculating what we can
## ------------------------------------------------------------------------
dat_comp <- escalc(measure="SMD", 
                        m1i = get(col.int.means),
                        sd1i = get(col.int.sds),  
                        n1i = get(col.int.ns),  
                        m2i = get(col.cont.means),
                        sd2i = get(col.cont.sds),  
                        n2i = get(col.cont.ns),  
                        data=dat_comp)

dat_comp

###This is our starting meta-analysis based on complete cases
## ------------------------------------------------------------------------
dat_MA <- rma(yi, vi, data = dat_comp)
dat_MA
forest(dat_MA)


### Try and calculate t-values from available exact p-values
## ------------------------------------------------------------------------
dat_comp$tval <- replmiss(dat_comp$tval, with(dat_comp, sign * qt(pval/2, df=get(col.int.ns)+get(col.cont.ns)-2, lower.tail=FALSE)))
dat_comp

## Try and calculate d-values from available t-values
## ------------------------------------------------------------------------
dat_comp$dval <- replmiss(dat_comp$dval, 
                          with(dat_comp, 
                               tval * sqrt(1/get(col.int.ns) + 1/get(col.cont.ns))))
dat_comp

### Convert from Cohen's d to hedges g
## ------------------------------------------------------------------------
dat_comp$yi <- replmiss(dat_comp$yi, 
                        with(dat_comp, 
                             (1 - 3/(4*(get(col.int.ns)+get(col.cont.ns)-2) - 1)) * dval))
dat_comp

### Calculate missing sampling variances
## ------------------------------------------------------------------------
dat_comp$vi <- replmiss(dat_comp$vi, 
                        with(dat_comp, 
                             1/get(col.int.ns) + 1/get(col.cont.ns) + yi^2/(2*(get(col.int.ns)+get(col.cont.ns)))))
dat_comp

## This is our meta-analysis based on compiled data
## ------------------------------------------------------------------------
dat_MA <- rma(yi, vi, data = dat_comp)
dat_MA
forest(dat_MA)

## Write the data to a file
## ------------------------------------------------------------------------
write.csv(dat_comp, "compiled_data.csv", row.names = FALSE)

