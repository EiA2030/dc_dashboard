FROM rocker/shiny:4.2

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Could be unnecessary
RUN apt-get install libmysqlclient-dev -y

# OS dependencies
RUN apt-get install libgdal-dev -y
RUN apt-get install libharfbuzz-dev libfribidi-dev -y
RUN apt-get install libsodium-dev libudunits2-dev -y
RUN apt-get install cron -y
RUN apt-get install bzip2 -y
RUN apt-get install libgit2-dev libssh2-1-dev librsvg2-dev -y
RUN apt-get install nano -y

WORKDIR /app

COPY . .

# Copy the cron file to the container
RUN mv cronjobs/cronfile /etc/cron.d/cronfile

# Give execution rights on the cron job
RUN chmod 0644 /etc/cron.d/cronfile

# Apply the cron job
RUN crontab /etc/cron.d/cronfile

RUN Rscript libraries.R
RUN Rscript dataprocessing.R

RUN (crontab -l ; echo "0 0 * * * Rscript /app/dataprocessing.R >> /var/log/cron.log") | crontab

CMD ["R", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=80)", "&&", "cron", "-f"]