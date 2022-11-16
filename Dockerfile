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
RUN R -e "install.packages(\"cronR\", repos = \"http://cran.us.r-project.org\")"
WORKDIR /root
COPY . .

# Add cron job and set environment variables for it
RUN Rscript cronfile.R
RUN crontab -l > /tmp/odys_old_cron_file.txt
RUN cat _.env /tmp/odys_old_cron_file.txt > /tmp/odys_new_cron_file.txt
RUN crontab /tmp/odys_new_cron_file.txt

EXPOSE 80
CMD /root/start_script.sh
