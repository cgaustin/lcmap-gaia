FROM ubuntu:18.04
MAINTAINER USGS LCMAP http://eros.usgs.gov

RUN apt-get update
RUN apt-get install leiningen openjdk-8-jre curl vim -y
RUN apt-get install gdal-bin libgdal-dev libgdal-java libgdal20 python-gdal -y
RUN mkdir -p /app
WORKDIR /app
COPY . /app

RUN cd resources; tar xvf *tar.gz;
RUN lein deps; lein uberjar

CMD /app/bin/startup.sh

