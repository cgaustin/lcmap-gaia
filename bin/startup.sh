#!/bin/bash
version=`bin/version`

if [ -z ${BUNDLE_USER} ]
  then
    export BUNDLE_USER=root
else
  useradd -u ${BUNDLE_USER_ID} ${BUNDLE_USER}
fi

if [ -z ${WORK_DIR} ]
  then
    export WORK_DIR=/app
fi

chown ${BUNDLE_USER}:${BUNDLE_USER} ${WORK_DIR}

cd ${WORK_DIR}
cp -r /app/templates .

echo "working from: ${WORK_DIR}, as user: ${BUNDLE_USER}..."

su ${BUNDLE_USER} -c "java -server -XX:+UseG1GC -Djava.io.tmpdir=$WORK_DIR -jar /app/target/gaia-$version-standalone.jar"

