FROM rocker/shiny:4.2

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Could be unnecessary
RUN apt-get install libmysqlclient-dev -y

# OS dependencies
RUN apt-get install libgdal-dev libharfbuzz-dev libfribidi-dev libsodium-dev libudunits2-dev libgit2-dev libssh2-1-dev librsvg2-dev cron nano bzip2 -y

WORKDIR /app

COPY . .

RUN Rscript libraries.R
RUN Rscript dataprocessing.R

RUN (crontab -l ; echo "0 0 * * * Rscript /app/dataprocessing.R >> /var/log/cron.log") | crontab

CMD ["R", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=80)"]