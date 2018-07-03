## ----echo=FALSE----------------------------------------------------------
Author <- "Your Name"
Rev.Title <- "Review Title"
filename.ES.Data<- "alt.csv"
filename.PRISMA.Data<- "prisma.csv"
X<- "Anxiety"
Y <- "Exercise"
Z <- "Waitlist Control"


### Specify the columns containing the Study ID and means, SDs, and Ns for intervention and control within the file

col.int.means     <- "m1"
col.int.sds       <- "sd1"
col.int.ns        <- "n1"
col.cont.means    <- "m2"
col.cont.sds      <- "sd2"
col.cont.ns       <- "n2"
col.study.id      <- "StudyID"


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
knitr::opts_chunk$set(echo = TRUE)
library(metafor)
library(knitr)
library(RCurl)
library(dplyr)
library(rmarkdown)
library(DiagrammeR)
#library(DiagrammeRsvg)
#library(magrittr)
#library(svglite)
#library(rsvg)
#library(png)


output<-default_output_format(knitr::current_input()) ## This will fail if run manually. Don't worry!


## These lines of code download and run the metafor_tidiers functions that implement broom type tidy data functions for rma objects
source("metafor_tidiers.R")

## This opens the PRISMA function adapted from the prismastatement package
source("prisma.R")

# Set so that long lines in R will be wrapped:
opts_chunk$set(tidy.opts=list(width.cutoff=80),tidy=TRUE)

prisma.data <- read.csv(filename.PRISMA.Data)

## ----calculate_ES, include=FALSE-----------------------------------------
dat <- read.csv(filename.ES.Data, stringsAsFactors = FALSE)
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


## ----conduct_MA, echo=FALSE----------------------------------------------
dat_MA<- rma(yi, vi, data=dat_ES, slab=get(col.study.id))
model<-tidy.rma(dat_MA)
het.small<-glance.rma(dat_MA) %>% select(one_of(c("k", "tau2", "se.tau2", "QE", "QEp", "I2")))
eggers<-regtest(dat_MA)


## ----prisma_png, fig.cap="Figure 1. PRISMA Flowchart", echo=FALSE, warning=FALSE, out.width="400px", message=FALSE, eval=(output$name!="html_document")----
prisma_graph<-prisma(found = prisma.data$found,
        found_other = prisma.data$found_other,
        no_dupes = prisma.data$no_dupes, 
        screened = prisma.data$screened, 
        screen_exclusions = prisma.data$screened_exclusions, 
        full_text = prisma.data$full_text,
        full_text_exclusions = prisma.data$full_text_exclusions, 
        qualitative = prisma.data$qualitative,
        quantitative = prisma.data$quantitative,
        reasons = paste(prisma.data$reasons))

# Create a PNG of this graph
export_svg(prisma_graph) %>% charToRaw %>% rsvg %>% png::writePNG('prisma_graph.png')

knitr::include_graphics('graph2.png')

## ----prisma_html, fig.cap="Figure 1. PRISMA Flowchart", echo=FALSE, warning=FALSE, message=FALSE, eval=(output$name=="html_document")----

prisma(found = prisma.data$found,
        found_other = prisma.data$found_other,
        no_dupes = prisma.data$no_dupes, 
        screened = prisma.data$screened, 
        screen_exclusions = prisma.data$screened_exclusions, 
        full_text = prisma.data$full_text,
        full_text_exclusions = prisma.data$full_text_exclusions, 
        qualitative = prisma.data$qualitative,
        quantitative = prisma.data$quantitative,
        reasons = paste(prisma.data$reasons))

## ----forest, echo=FALSE, warning=FALSE, fig.height=het.small$k*0.5, fig.cap="Figure 2. Forest Plot"----
forest(dat_MA)

## ----fig.cap="Figure 3. Funnel Plot", echo=FALSE-------------------------
funnel<-funnel(dat_MA, back="white")

## ----include=FALSE-------------------------------------------------------
citPkgs <- names(sessionInfo()$otherPkgs)
write_bib(citPkgs, file="R-Pckgs.bib")

