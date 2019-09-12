#!/bin/bash

set -e

REGISTRY_USERNAME=$1
REGISTRY_PASSWORD=$2
IMAGE_NAME=$3
TAG=$4

if [ -z $REGISTRY_USERNAME ]; then
    echo "Please provide a username to push to docker repo"
    exit 1
elif [ -z $REGISTRY_PASSWORD ]; then
    echo "Please provide a password to push to docker repo"
    exit 1
elif [ -z $IMAGE_NAME ]; then
    echo "Please provide an image name to push to docker repo"
    exit 1    
elif [ -z $TAG ]; then
    echo "Please provide a tag to push to docker repo"
    exit 1
fi

docker login --username "${REGISTRY_USERNAME}" --password "${REGISTRY_PASSWORD}"
docker push ${REGISTRY_USERNAME}/${IMAGE_NAME}:${TAG}
