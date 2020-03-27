#!/bin/bash
version=`bin/version`
cd ${WORK_DIR}
cp -r /app/templates .
java -server -Xms$Xms -Xmx$Xmx -XX:+UseG1GC -jar /app/target/gaia-$version-standalone.jar
