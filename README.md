[![Build Status](https://travis-ci.org/USGS-EROS/lcmap-gaia.svg?branch=develop)](https://travis-ci.org/USGS-EROS/lcmap-gaia)

# Gaia

Gaia provides three services:
     1) Producing Change and Classification product values from CCDC results.
     2) Generating Change and Classification product rasters.
     3) Assembling and Delivering product tarballs for distribution.

Product value json and rasters are persisted to the configured Object Storage service.


## Deploying Gaia

Gaia is run as a Docker container. 

```
export CCD_VERSION="v01"
export CHIPMUNK_HOST="http://awesomehost.org/chipmunk"
export CHIPMUNK_ACQUIRED="1999-01-01/2002-01-01"
export COLLECTION="01"
export HTTP_PORT=9876
export NEMO_HOST="http://awesomehost.org/nemo"
export NEMO_TIMEOUT=2400000
export OBSERVATIONS_PATH="/chip"
export PREDICTIONS_PATH="/conus_predictions"
export QUERY_DAY="07-01"
export REGION="cu"
export RETRY_STRATEGY="5000 15000 30000"
export SEGMENTS_PATH="/conus_segments"
export STABILITY_BEGIN="1982-01-01"
export STORAGE_ACCESS_KEY="9876asdrd"
export STORAGE_BUCKET="some-ceph-bucket"
export STORAGE_ENDPOINT="http://localhost:7480"
export STORAGE_LOCATION="/data/ccdc"
export STORAGE_SECRET_KEY="13235lkjis"
export Xms=4352m
export Xmx=4352m

docker run -p 9876:${HTTP_PORT} -e NEMO_HOST=${NEMO_HOST} \
                                -e CHIPMUNK_HOST=${CHIPMUNK_HOST} \
                                -e CHIPMUNK_ACQUIRED=${CHIPMUNK_ACQUIRED} \
                                -e SEGMENTS_PATH=${SEGMENTS_PATH} \
                                -e PREDICTIONS_PATH=${PREDICTIONS_PATH} \
                                -e STORAGE_ENDPOINT=${STORAGE_ENDPOINT} \
                                -e STORAGE_ACCESS_KEY=${STORAGE_ACCESS_KEY} \
                                -e STORAGE_SECRET_KEY=${STORAGE_SECRET_KEY} \
                                -e STORAGE_BUCKET=${STORAGE_BUCKET} \
                                -e STORAGE_LOCATION=${STORAGE_LOCATION}
                                -e HTTP_PORT=${HTTP_PORT} \
                                -e REGION=${REGION} \
                                -e CCD_VERSION=${CCD_VERSION} \
                                -e Xmx=${Xmx} \
                                -e Xms=${Xms} \
                                -it usgseros/lcmap-gaia:latest
```

Gaia is configured using these environment variables:

| ENV                  | Description                            |
|----------------------|----------------------------------------|
| `CCD_VERSION`        | version of ccd algorithm used to       |
|                      | generate input data                    |
| `CHIPMUNK_HOST`      | base url for lcmap-chipmunk resource   |
| `CHIPMUNK_ACQUIRED`  | acquired value for requesting aux data |
| `COLLECTION`         | collection number of source data       |
| `HTTP_PORT`          | HTTP port to expose the server at      |
| `NEMO_HOST`          | base url for lcmap-nemo resource       |
| `NEMO_TIMEOUT`       | timeout for Nemo requests              |
| `OBSERVATIONS_PATH`  | resource for observations data         |
| `PREDICTIONS_PATH`   | resource path for prediction data      |
| `QUERY_DAY`          | date for product value calculations    |
| `REGION`             | region abbreviation (cu, ak, hi)       |
| `RETRY_STRATEGY`     | retry strategy for data requests       |
| `SEGMENTS_PATH`      | resource path for segments data        |
| `STABILITY_BEGIN`    | start date length-of-segment product   |
| `STORAGE_ACCESS_KEY` | access key for object storage service  |
| `STORAGE_BUCKET`     | name of the object store bucket to use |
| `STORAGE_ENDPOINT`   | url for object storage service         |
| `STORAGE_LOCATION`   | file system location to place products |
| `STORAGE_SECRET_KEY` | secret key for object storage service  |
| `Xms`                | minimum JVM memory                     |
| `Xmx`                | maximum JVM memory                     |


## Running a local Gaia

Using docker-compose, build a container to provide sample json, and a gaia instance

```
docker-compose -f resources/docker-compose.yml up

```

## Running tests

```
lein test
```

## All examples use HTTPie for making http requests https://httpie.org
##
## Requesting product calculation
```
http POST 127.0.0.1:9876/product cx="1484415" \
                                 cy="2114805" \
                                 dates:="[\"2006-07-01\"]" \
                                 tile="003008" \
                                 products:="[\"change\", \"cover\"]"
```

## Requesting raster creation
```
http POST 127.0.0.1:9876/raster date="2006-07-01" \
                                tile="003008" \
                                tilex="1484415" \
                                tiley="2114805" \
                                products:="change" \
                                chips:="[{\"cx\":\"1484415\", \"cy\":\"2114805\"},...]"
```

## Requesting product tar bundle
```
http POST 127.0.0.1:9876/bundle years="1985/2017" \
                                tile="003008"
```

## Jupyter Notebook with Clojure kernel
```
(require '[cemerick.pomegranate :as pom])
(pom/add-classpath "/workspace/lcmap-gaia/target/gaia-0.2.7-standalone.jar")
```
