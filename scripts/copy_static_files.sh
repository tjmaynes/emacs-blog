#!/bin/bash

set -e

DESTINATION_DIRECTORY=$1

if [ -z $DESTINATION_DIRECTORY ]; then
    echo "Please provide a destination directory to copy files to."
    exit 1
fi

FILES=(favicon.ico keybase.txt CNAME)

[ -d $DESTINATION_DIRECTORY ] || mkdir -p $DESTINATION_DIRECTORY

for file in "${FILES[@]}"; do
    echo "Copying file static/$file to directory $DESTINATION_DIRECTORY/$file"
    cp -rf static/$file $DESTINATION_DIRECTORY/$file
done
