#!/bin/bash

set -e

REGISTRY_USERNAME=$1
IMAGE_NAME=$2
TAG=$3
VOLUME_DIRECTORY=$4

if [[ -z $REGISTRY_USERNAME ]]; then
    echo "Please provide a username for debugging the docker image"
    exit 1
elif [[ -z $IMAGE_NAME ]]; then
    echo "Please provide an image_name for debugging the docker image"
    exit 1
elif [[ -z $TAG ]]; then
    echo "Please provide a tag for debugging the docker image"
    exit 1
elif [[ -z $VOLUME_DIRECTORY ]]; then
    echo "Please provide a volume directory for debugging the docker image"
    exit 1
fi

docker run \
    --rm -it \
    --workdir /src \
    --volume $VOLUME_DIRECTORY:/src \
    $REGISTRY_USERNAME/$IMAGE_NAME:$TAG \
    publish_blog BLOG_DIRECTORY=/src/
