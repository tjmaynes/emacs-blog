#!/bin/bash

set -e

SOURCE_DIRECTORY=$1
DESTINATION_DIRECTORY=$2

if [ -z $SOURCE_DIRECTORY ]; then
    echo "Please provide a source directory to copy files from."
    exit 1
elif [ -z $DESTINATION_DIRECTORY ]; then
    echo "Please provide a destination directory to copy files to."
    exit 1
fi

FILES=(favicon.ico keybase.txt CNAME)

[ -d $DESTINATION_DIRECTORY ] || mkdir -p $DESTINATION_DIRECTORY

for file in "${FILES[@]}"; do
    echo "Copying file static/$file to directory $DESTINATION_DIRECTORY/$file"
    cp -rf $SOURCE_DIRECTORY/$file $DESTINATION_DIRECTORY/$file
done
