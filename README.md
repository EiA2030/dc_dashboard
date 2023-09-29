# DC Dashboard


### Deployemnt

`dataprocessing.R` script is executed once during the build process in order to generate the needed files for the Shiny app and then a relevant CronJob is applied on the host (non-root) for executing it once per day:

`0 0 * * * docker exec -it eia2030-dc-dashboard Rscript dataprocessing.R; docker stop eia2030-dc-dashboard; docker start eia2030-dc-dashboard`