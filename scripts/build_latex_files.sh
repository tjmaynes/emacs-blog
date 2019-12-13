#!/bin/bash

set -e

DESTINATION_DIRECTORY=$1

if [ -z $DESTINATION_DIRECTORY ]; then
    echo "Please provide a destination directory to copy files to."
    exit 1
fi

[ -d $DESTINATION_DIRECTORY/career ] || mkdir -p $DESTINATION_DIRECTORY/career


function build_career_files()
{
    FILES=(cv resume)

    cd career
    
    for file in "${FILES[@]}"; do
	echo "Compiling LaTeX file $file to directory $DESTINATION_DIRECTORY/career"
	xelatex -output-directory=../$DESTINATION_DIRECTORY/career $file.tex
	rm -rf ../$DESTINATION_DIRECTORY/career/$file.log
    done
}

build_career_files
