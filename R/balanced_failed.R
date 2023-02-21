# Have a look at models that failed 

rm(list=ls())

# Load libraries  ---------------------------------------------------------

library(tidyverse)
library(vroom)
library(readr)
library(data.table)


# Set working directory ---------------------------------------------------

# W.r.t. path where this script is saved 
cd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(cd)



# Get models with error  --------------------------------------------------

# Based on how the data is collected and the analysis is run on the hpc, 
# there ar some output measures for which there will be missing data, 
# indicating something went wrong in the model fit. 


## Check where we miss information ----

# Get all files in the output folder 
all_files <- list.files("../output", pattern = ".csv", recursive = T, all.files = F, full.names = T)

# Order according to nth_data (use basenames becasue the ".." gives error in parse_number)
all_files <- all_files[order(readr::parse_number(basename(all_files)))]

# Get the path until last separator 
dir_list <- split(all_files, dirname(all_files))

# Get the number of files in each folder 
files_in_folder <- sapply(dir_list, length)

# Get folders where n < 4050 
n_data <- 4050
folder_where_missing <- files_in_folder[files_in_folder < n_data]
folder_where_missing


## Check which nth_data are missing for each of the measures in "folder_where_missing" ----

nth_data_missing_list <- list()
for(p in names(folder_where_missing)){
  nth_data <- readr::parse_number(list.files(p))
  nth_data <- c(1:n_data)[!1:n_data %in% nth_data]
  nth_data_missing_list[[p]] <- nth_data
}

all(nth_data_missing_list$`../output/hyperpar/precision/pmarg` %in%
      nth_data_missing_list$`../output/hyperpar/precision/summary_pmarg`)
all(nth_data_missing_list$`../output/hyperpar/precision/pmarg` %in%
      nth_data_missing_list$`../output/hyperpar/standard_deviation/internal/pmarg`) # Not true when internal checking 
all(nth_data_missing_list$`../output/hyperpar/precision/pmarg` %in%
      nth_data_missing_list$`../output/hyperpar/standard_deviation/pmarg`)

# Most inmportant checks 
all(nth_data_missing_list$`../output/hyperpar/precision/pmarg` %in%
      nth_data_missing_list$`../output/hyperpar/precision/summary_pmarg`)
all(nth_data_missing_list$`../output/hyperpar/precision/pmarg` %in%
      nth_data_missing_list$`../output/hyperpar/standard_deviation/pmarg`)
all(nth_data_missing_list$`../output/hyperpar/precision/summary_pmarg` %in%
      nth_data_missing_list$`../output/hyperpar/standard_deviation/summary_pmarg`)
all(nth_data_missing_list$`../output/hyperpar/standard_deviation/pmarg` %in%
      nth_data_missing_list$`../output/hyperpar/standard_deviation/summary_pmarg`)

# = 27 
sum(nth_data_missing_list$`../output/hyperpar/standard_deviation/summary_pmarg` %in%
      nth_data_missing_list$`../output/hyperpar/precision/summary_pmarg`) == 27


### Conclusion ---- 

# Models where the transformation precision -> standard_deviation failed, i.e.,
# where standard_deviation files are missing (N = 68), something went wrong and
# have to be checked

# For some models we have precision ouptut but nog standard deviaton output 


## Models with an error ---- 
models_with_error <- nth_data_missing_list$`../output/hyperpar/standard_deviation/pmarg`
files_error <- all_files[readr::parse_number(basename(all_files))%in%models_with_error]


# Failed models  ----------------------------------------------------------

## Check output model checks  ----
files_error_other <- files_error[grep("other",files_error)]

### log output ----
files_error_other_logfile <- files_error_other[grep("logfile",files_error_other)]
# files_error_other_logfile <- paste0("../output/other/logfile/",models_with_error,".csv")
hessian_error <- vroom(files_error_other_logfile) %>% 
  mutate(hessian = ifelse(stringr::str_detect(logfile,"trouble with the Hessian"),1,0))

table(hessian_error$hessian)


### mode.status  -----
files_error_other_mode.status <- files_error_other[grep("mode_status",files_error_other)]
# files_error_other_logfile <- paste0("../output/other/logfile/",models_with_error,".csv")
mode.status_error <- vroom(files_error_other_mode.status) 


### ok  ----
files_error_other_ok <- files_error_other[grep("ok",files_error_other)]
# files_error_other_logfile <- paste0("../output/other/logfile/",models_with_error,".csv")
ok_error <- vroom(files_error_other_ok) 




## Standard deviation (transformation of precision into standard deviation) ----

rdir_sd <- "../output/hyperpar/standard_deviation/"

# summary should be empty (no summary computed by inla for sd)
is_empty(list.files(paste0(rdir_sd,"summary")))











