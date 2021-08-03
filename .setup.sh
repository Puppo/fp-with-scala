#!/bin/bash
set -e
docker-compose pull
docker-compose build
docker-compose rm -v --force
docker-compose up -d --force-recreate
docker-compose stop
