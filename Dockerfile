FROM rocker/shiny:4.2

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

RUN apt-get update
RUN apt-get install libmysqlclient-dev -y
RUN apt-get install libgdal-dev -y

# Necessary
RUN apt-get install libharfbuzz-dev libfribidi-dev -y
RUN apt-get install libsodium-dev libudunits2-dev -y

RUN apt-get install cron -y
RUN apt-get install bzip2 -y
RUN apt-get install libgit2-dev libssh2-1-dev -y

WORKDIR /workdir
COPY libraries.R /workdir/libraries.R
RUN Rscript /workdir/libraries.R

COPY . /workdir/

RUN R -e "install.packages(\"cronR\", repos = \"http://cran.us.r-project.org\")"
RUN "export $(grep -v '^#' _.env | xargs)"
RUN Rscript /workdir/cronfile.R

EXPOSE 80
CMD ["R", "-e", "shiny::runApp('/workdir/app.R', host='0.0.0.0', port=80)"]
