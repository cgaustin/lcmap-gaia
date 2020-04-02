FROM ubuntu:18.04
MAINTAINER USGS LCMAP http://eros.usgs.gov

RUN apt-get update
RUN apt-get install leiningen openjdk-8-jre curl vim -y
RUN apt-get install gdal-bin libgdal-dev libgdal-java libgdal20 python-gdal -y
RUN mkdir -p /app
WORKDIR /app
COPY . /app

RUN cd resources; gunzip *gz;  tar -xvf *tar;
RUN lein deps; lein uberjar

# CCDC users for delivering build products
RUN useradd -u 17022 ccdcops; useradd -u 17021 ccdcst; useradd -u 17020 ccdcit

CMD /app/bin/startup.sh

