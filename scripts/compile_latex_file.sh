#!/bin/bash

set -e

FILE_LOCATION=$1
FILE_DESTINATION=$2

if [ -z $FILE_LOCATION ]; then
    echo "Please provide the location of the latex file"
    exit 1
elif [ -z $FILE_DESTINATION ]; then
    echo "Please provide a destination to send latex file"
    exit 1
fi

FILE_DIRECTORY=$(dirname "${FILE_LOCATION}")
FILE=$(basename "${FILE_LOCATION}")
FILENAME="${FILE%.*}"

mkdir -p ${FILE_DESTINATION} || true
cd ${FILE_DIRECTORY}
xelatex \
    -output-directory=../${FILE_DESTINATION} \
    ${FILE}

echo "Removing LaTeX aux files..."
rm -f ../${FILE_DESTINATION}/${FILENAME}.log && \
rm -f ../${FILE_DESTINATION}/${FILENAME}.aux && \
rm -f ../${FILE_DESTINATION}/${FILENAME}.out
