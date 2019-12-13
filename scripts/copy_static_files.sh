
#!/bin/bash

set -e

DESTINATION_DIRECTORY=$1

if [ -z $DESTINATION_DIRECTORY ]; then
    echo "Please provide a destination directory to copy files to."
    exit 1
fi

FILES=(static/favicon.ico static/keybase.txt static/CNAME)

[ -d $DESTINATION_DIRECTORY/static ] || mkdir -p $DESTINATION_DIRECTORY/static

for file in "${FILES[@]}"; do
    echo "Copying file $file to directory $DESTINATION_DIRECTORY/static"
    cp -rf $file $DESTINATION_DIRECTORY/$file
done
