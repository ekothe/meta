---
title: "R Notebook"
output: html_notebook
---

## First let's get the most recent version of the data from Google Sheets

```{r}
## This script downloads data that has been extracted by coding teams during a data extraction activity
## Minimal data cleaning is performed to allow for analysis

## Load required packages

library("googlesheets")
library("knitr")

## Import data from Google Sheets

key<-gs_key("1HOnqlfCHHo8NAQFeh8EzIIUr56fYaMsb4DHigZafNig")
data_import<-gs_read(key)

## Clean data and create standard column names
data_cleaned<-data_import

data_cleaned$Study.ID<-toupper(data_import$`Study ID`)
data_cleaned$Intervention.Mean<-as.numeric(data_import$`Intervention Mean`)
data_cleaned$Intervention.N<-as.numeric(data_import$`Intervention N`)
data_cleaned$Intervention.SD<-as.numeric(data_import$`Intervention SD`)
data_cleaned$Control.Mean<-as.numeric(data_import$`Control Mean`)
data_cleaned$Control.N<-as.numeric(data_import$`Control N`)
data_cleaned$Control.SD<-as.numeric(data_import$`Control SD`)

## Drop unnecessary columns from working data file
data<-data_cleaned[,c("Timestamp","Study.ID", "Intervention.Mean", "Intervention.SD", "Intervention.N", "Control.Mean", "Control.SD", "Control.N")]

kable(data)
```


