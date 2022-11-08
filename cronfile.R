
#load/install cronR
if(!require(cronR)) install.packages("cronR", repos = "http://cran.us.r-project.org")

#library(cronR)

r <- cron_rscript("dataprocessing.R")

cron_add(r, frequency = "daily",id="job_1", at = "00:00", description = "update_data",ask = FALSE)


#cron_clear()