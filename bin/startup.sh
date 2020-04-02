#!/bin/bash
version=`bin/version`

if [ -z ${BUNDLE_USER} ]
  then
    export BUNDLE_USER=root
fi

if [ -z ${WORK_DIR} ]
  then
    export WORK_DIR=/app
fi

chown ${BUNDLE_USER}:${BUNDLE_USER} ${WORK_DIR}
su ${BUNDLE_USER}

cd ${WORK_DIR}
cp -r /app/templates .

echo "working from: ${WORK_DIR}, as user: ${BUNDLE_USER}..."

java -server -Xms$Xms -Xmx$Xmx -XX:+UseG1GC -jar /app/target/gaia-$version-standalone.jar
