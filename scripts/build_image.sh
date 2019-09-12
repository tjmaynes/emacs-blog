#!/bin/bash

set -e

REGISTRY_USERNAME=$1
IMAGE_NAME=$2
TAG=$3

if [ -z $REGISTRY_USERNAME ]; then
    echo "Please provide a username for building the docker image"
    exit 1
elif [ -z $IMAGE_NAME ]; then
    echo "Please provide an image_name for building the docker image"
    exit 1
elif [ -z $TAG ]; then
    echo "Please provide a tag for building the docker image"
    exit 1
fi

docker build -t ${REGISTRY_USERNAME}/${IMAGE_NAME}:${TAG} .
