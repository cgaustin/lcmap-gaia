[![Build Status](https://travis-ci.org/USGS-EROS/lcmap-gaia.svg?branch=develop)](https://travis-ci.org/USGS-EROS/lcmap-gaia)

# Gaia

Gaia provides three services:
     1) Producing Change and Classification product values from CCDC results.
     2) Generating Change and Classification product rasters.
     3) Assembling and Delivering product tarballs for distribution.

Product value json and rasters are persisted to the configured Object Storage service.


## Deploying Gaia

Gaia is run as a Docker container. 

Minimum configuration example:
```
export CCD_VERSION="v01"
export CHIPMUNK_HOST="http://awesomehost.org/chipmunk"
export CHIPMUNK_ACQUIRED="1999-01-01/2002-01-01"
export COLLECTION="01"
export HTTP_PORT=9876
export OBSERVATIONS_PATH="/chip"
export PREDICTIONS_PATH="/conus_predictions"
export QUERY_DAY="07-01"
export REGION="CU"
export RETRY_STRATEGY="5000 15000 30000"
export SEGMENTS_PATH="/conus_segments"
export STABILITY_BEGIN="1982-01-01"
export STORAGE_ACCESS_KEY="9876asdrd"
export STORAGE_BUCKET="some-ceph-bucket"
export STORAGE_ENDPOINT="http://localhost:7480"
export STORAGE_LOCATION="/data/ccdc"
export STORAGE_SECRET_KEY="13235lkjis"
export Xmx=8192m


docker run -p 9876:${HTTP_PORT} -e CCD_VERSION=${CCD_VERSION} \
                                -e CHIPMUNK_ACQUIRED=${CHIPMUNK_ACQUIRED} \
                                -e CHIPMUNK_HOST=${CHIPMUNK_HOST} \
                                -e HTTP_PORT=${HTTP_PORT} \
                                -e PREDICTIONS_PATH=${PREDICTIONS_PATH} \
                                -e SEGMENTS_PATH=${SEGMENTS_PATH} \
                                -e STORAGE_ENDPOINT=${STORAGE_ENDPOINT} \
                                -e STORAGE_ACCESS_KEY=${STORAGE_ACCESS_KEY} \
                                -e STORAGE_SECRET_KEY=${STORAGE_SECRET_KEY} \
                                -e STORAGE_BUCKET=${STORAGE_BUCKET} \
                                -e STORAGE_LOCATION=${STORAGE_LOCATION}
                                -e HTTP_PORT=${HTTP_PORT} \
                                -e REGION=${REGION} \
                                -e CCD_VERSION=${CCD_VERSION} \
                                -e Xmx=${Xmx} \
                                -it usgseros/lcmap-gaia:latest
```

Gaia is configured using these environment variables:

| ENV                   | Description                                 | Default
|-----------------------|---------------------------------------------|--------------------------------
| `BUNDLE_USER`         | optional user added to the container to own | 
|                       | generated files                             | root
| `BUNDLE_USER_ID`      | userid for optional BUNDLE_USER             |
| `CCD_VERSION`         | version of ccd algorithm used to            | 
|                       | generate input data                         |
| `CHIPMUNK_ACQUIRED`   | acquired value for requesting aux data      | "1999-01-01/2002-01-01"
| `CHIPMUNK_HOST`       | base url for lcmap-chipmunk resource        |
| `COLLECTION`          | collection number of source data            |
| `FILL_DIFFLC`         | fill between different landcover values     | true                                
| `FILL_SAMELC`         | fill between same landcover values          | true
| `HTTP_PORT`           | HTTP port to expose the server at           | 9876
| `LC_AFTERBREAK`       | confidence value for after break            | 214
| `LC_AG`               | cover value for agriculture                 | 2
| `LC_BACK`             | confidence value for fill back              | 213
| `LC_BARREN`           | cover value for barren                      | 8
| `LC_DECLINE`          | confidence value for decline                | 152
| `LC_DEVELOP`          | cover value for developed                   | 1
| `LC_DIFFLC`           | confidence value for different cover values | 212
| `LC_FORWARDS`         | confidence value for fill forward           | 202
| `LC_GRASS`            | cover value for grass                       | 3
| `LC_GROWTH`           | confidence value for growth                 | 151
| `LC_NOMODEL`          | confidence value for no model               | 201
| `LC_NONE`             | cover value for 'none'                      | 0
| `LC_SAMELC`           | confidence value for same cover values      | 211
| `LC_SNOW`             | cover value for snow                        | 7
| `LC_TREE`             | cover value for trees                       | 4
| `LC_WATER`            | cover value for water                       | 5
| `LC_WETLAND`          | cover value for wetland                     | 6
| `LCMAP_ENV`           | to denote 'test' environment for storage    |
| `OBSERVATIONS_PATH`   | resource for observations data              |
| `OBSERVATION_BEGIN`   | first year of processed observations        | 1985
| `OBSERVATION_END`     | last year of processed observations         | 2017 
| `PREDICTIONS_PATH`    | resource path for prediction data           |
| `QUERY_DAY`           | date to calculate product values for        | "07-01"
| `REGION`              | region abbreviation (cu, ak, hi)            |
| `RETRY_STRATEGY`      | wait times for subsequent retries [ms,]     | [5000 15000 30000]
| `SEGMENTS_PATH`       | resource path for segment data              |
| `STABILITY_BEGIN`     | start of collection data                    | "1982-01-01"
| `STORAGE_ACCESS_KEY`  | access key for object storage service       |
| `STORAGE_BUCKET`      | name of the object store bucket to use      |
| `STORAGE_DESTINATION` | name of object store bucket to persist to   | ${STORAGE_BUCKET}
| `STORAGE_ENDPOINT`    | url for object storage service              |
| `STORAGE_LOCATION`    | file system location to place products      |
| `STORAGE_SECRET_KEY`  | secret key for object storage service       |
| `TIFF_COMPRESS_COUNT` | simultaneous compression operations         | 2
| `WORK_DIR`            | dir within container to run the jar,        | /
|                       | temporary files are kept here               | (used only by startup.sh in container)
| `Xmx`                 | max JVM memory                              | 

## Local storage considerations

Docker containers exist under the /var/lib/docker/containers dir on the host system.  
Temporary files created by lcmap-gaia during raster creation will be written under 
the container specific location within that directory. 

If you define the $WORK_DIR env variable, and mount a host folder to this location 
within the container, temporary files will be operated on at this mount point.

For example, say you have some high performance SSD mounted locally at /mnt/mesos/sandbox
where you'd prefer temp files be placed.  You could mount that local directory to
/sandbox in your container and define WORK_DIR=/sandbox in your docker run command.


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

## Contributing
Before committing to this repository, run the following command.

```bash
git config --local commit.template .gitmessage
```

This will add the LCMAP commit template to `git commit`.

```text
jira/lcmap-xxxx: Title
Description
```
