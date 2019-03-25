[![Build Status](https://travis-ci.org/USGS-EROS/lcmap-gaia.svg?branch=develop)](https://travis-ci.org/USGS-EROS/lcmap-gaia)

# Gaia

Gaia calculates Change and Classification product values from CCDC results.
Use it to generate change and classification product values, and to produce maps.

Results are persisted to the configured Object Storage service.


## Deploying Gaia

Gaia is run as a Docker container. 

```
export NEMO_HOST="http://awesomehost.org/nemo"
export SEGMENTS_PATH="/conus_segments"
export PREDICTIONS_PATH="/conus_predictions"
export STORAGE_ENDPOINT="http://localhost:7480"
export STORAGE_ACCESS_KEY="9876asdrd"
export STORAGE_SECRET_KEY="13235lkjis"
export REGION="cu"
export CCD_VERSION="v01"
export HTTP_PORT=9876

docker run -p 9876:${HTTP_PORT} -e NEMO_HOST=${NEMO_HOST} \
                                -e SEGMENTS_PATH=${SEGMENTS_PATH} \
                                -e PREDICTIONS_PATH=${PREDICTIONS_PATH} \
                                -e STORAGE_ENDPOINT=$(STORAGE_ENDPOINT) \
                                -e STORAGE_ACCESS_KEY=$(STORAGE_ACCESS_KEY) \
                                -e STORAGE_SECRET_KEY=$(STORAGE_SECRET_KEY) \
                                -e HTTP_PORT=${HTTP_PORT} \
                                -e REGION=${REGION} \
                                -e CCD_VERSION=${CCD_VERSION} \
                                -it usgseros/lcmap-gaia:latest
```

Gaia is configured using these environment variables:

| ENV                  | Description                           |
|----------------------|---------------------------------------|
| `NEMO_HOST`          | base url for lcmap-nemo resource      |
| `SEGMENTS_PATH`      | resource path for segments data       |
| `PREDICTIONS_PATH`   | resource path for prediction data     |
| `STORAGE_ENDPOINT`   | url for object storage service        |
| `STORAGE_ACCESS_KEY` | access key for object storage service |
| `STORAGE_SECRET_KEY` | secret key for object storage service |
| `REGION`             | region abbreviation (cu, ak, hi)      |
| `CCD_VERSION`        | version of ccd algorithm used to      |
|                      | generate input data                   |
| `HTTP_PORT`          | HTTP port to expose the server at     |

## Running a local Gaia

Using docker-compose, build a container to provide sample json, and a gaia instance

```
docker-compose -f resources/docker-compose.yml up

```

## Running tests

```
lein test
```

## Requesting products using HTTPie https://httpie.org
```
http POST 127.0.0.1:9876/products cx="1484415" \
                                  cy="2114805" \
                                  dates:="[\"2006-07-01\"]" \
                                  tile="003008" \
                                  product="time-since-change"
```

## Requesting a map using HTTPie
```
http POST 127.0.0.1:9876/maps date="2006-07-01" \
                              tile="003008" \
                              tilex="1484415" \
                              tiley="2114805" \
                              product="time-since-change" \
                              chips:="[{\"cx\":\"1484415\", \"cy\":\"2114805\"},...]"
```

## Jupyter Notebook with Clojure kernel
```
(require '[cemerick.pomegranate :as pom])
(pom/add-classpath "/workspace/lcmap-gaia/target/gaia-0.1.0-SNAPSHOT-standalone.jar")
```
