# Start cron service, otherwise it doesn't run
cron
# Run this once in the background so the dashboard will work a few minutes after deploying,
# instead of waiting for the cron job to run at midnight
Rscript dataprocessing.R >dataprocessing_startup_run.log 2>&1 &
# Start dashboard service
R -e "shiny::runApp('app.R', host='0.0.0.0', port=80)"