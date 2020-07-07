#!/bin/bash

set -e

PORT=$1
VOLUME_DIRECTORY=$2

if [[ -z $PORT ]]; then
    echo "Please provide a port for running the docker image"
    exit 1
elif [[ -z $VOLUME_DIRECTORY ]]; then
    echo "Please provide a volume directory for running the docker image"
    exit 1
fi

docker rm -f nginx-blog || true
docker run --name nginx-blog \
    --publish $PORT:80 \
    --publish 443:443 \
    --volume $VOLUME_DIRECTORY:/usr/share/nginx/html \
    nginx:1.15.7
