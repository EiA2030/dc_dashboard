cron
#Rscript dataprocessing.R
R -e "shiny::runApp('app.R', host='0.0.0.0', port=80)"
