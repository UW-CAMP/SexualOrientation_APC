### DEPRECATED

### This script takes large .XPT BRFSS files, assigns index numbers to each BRFSS observation, 
#          and generates .rds files for later use in the SOGI analysis.

### Prepare workspace ----- 
# clear environment
rm(list = ls())

# * call in packages ----
source("SOGI_00_packages.R")

# * define functions -----
tableNA <- function(x, ...){
   table(x, useNA = "ifany", ...)  
}

brfss <- list()
for (yr in 14:21) {
   cat('Loading year', yr, '\n')
   filename <- paste("data - raw/LLCP20", yr, ".XPT ", sep="")
   brfss[[yr]] <- read_xpt(filename)
   #note: the spaces after ".XPT" should be removed if running in a Windows Environment.
   brfss[[yr]]$index <- as.numeric(paste0(1:nrow(brfss[[yr]]), yr))
}

write_rds(brfss, "data - clean/brfss.rds")
write_rds(brfss[[14]], "data - clean/brfss14.rds")
write_rds(brfss[[15]], "data - clean/brfss15.rds")
write_rds(brfss[[16]], "data - clean/brfss16.rds")
write_rds(brfss[[17]], "data - clean/brfss17.rds")
write_rds(brfss[[18]], "data - clean/brfss18.rds")
write_rds(brfss[[19]], "data - clean/brfss19.rds")
write_rds(brfss[[20]], "data - clean/brfss20.rds")
write_rds(brfss[[21]], "data - clean/brfss21.rds")