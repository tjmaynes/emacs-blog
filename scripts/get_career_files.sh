#!/bin/bash

set -e

DESTINATION_DIRECTORY=$1

if [ -z $DESTINATION_DIRECTORY ]; then
    echo "Please provide a destination directory to copy files to."
    exit 1
fi

git clone \
    --single-branch \
    --branch release \
    https://github.com/tjmaynes/career \
    ${DESTINATION_DIRECTORY}/career