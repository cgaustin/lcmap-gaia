#!/bin/bash

mv /log4j.properties ${WORK_DIR}/log4j.properties
mv /gaia*jar ${WORK_DIR}/
cd ${WORK_DIR}
java -server -Xms$Xms -Xmx$Xmx -XX:+UseG1GC -jar ${WORK_DIR}/gaia-*-standalone.jar
