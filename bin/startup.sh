#!/bin/bash
version=`bin/version`
java -server -Xms$Xms -Xmx$Xmx -XX:+UseG1GC -jar /app/target/gaia-$version-standalone.jar
