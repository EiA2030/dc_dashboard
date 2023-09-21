FROM rocker/shiny:4.2

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Could be unnecessary
RUN apt-get install libmysqlclient-dev -y

# Necessary
RUN apt-get install libgdal-dev -y
RUN apt-get install libharfbuzz-dev libfribidi-dev -y
RUN apt-get install libsodium-dev libudunits2-dev -y
RUN apt-get install cron -y
RUN apt-get install bzip2 -y
RUN apt-get install libgit2-dev libssh2-1-dev librsvg2-dev -y

# This is because cron jobs run in root
WORKDIR /root
COPY libraries.R libraries.R
RUN Rscript libraries.R
COPY . .

# Add cron job
RUN Rscript cronfile.R

EXPOSE 80
# Script starts cron service, runs dataprocessing.R once, and starts shiny website.
RUN chmod +x /root/start_script.sh
RUN bash /root/start_script.sh
