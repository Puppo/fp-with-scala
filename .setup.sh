#!/bin/bash
set -e
docker-compose pull
docker-compose build
docker-compose rm -v --force
docker-compose up -d --force-recreate
docker exec -ti fp_with_scala_fp_dev sbt exit
docker-compose stop
