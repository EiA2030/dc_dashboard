


r <- cron_rscript("dataprocessing.R")

cron_add(r, frequency = "daily", at = "00:00", description = "update data")


