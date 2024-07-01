FROM rocker/shiny:4.2

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# OS dependencies
RUN apt-get install libmysqlclient-dev libgdal-dev libharfbuzz-dev libfribidi-dev libsodium-dev libudunits2-dev bzip2 libgit2-dev libssh2-1-dev librsvg2-dev -y

WORKDIR /app

COPY . .

RUN Rscript libraries.R
RUN Rscript dataprocessing.R

CMD ["R", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=80)"]