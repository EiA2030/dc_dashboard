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

RUN Rscript libraries.R

CMD ["R", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=80)"]