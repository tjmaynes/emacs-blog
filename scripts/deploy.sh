#!/bin/sh

set -e

BUILD_DIRECTORY=$1

if [ -z $BUILD_DIRECTORY ]
then
  echo "Which folder do you want to deploy to GitHub Pages?"
  exit 1
fi

git subtree push --prefix ${BUILD_DIRECTORY} origin gh-pages
