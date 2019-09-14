#!/bin/bash

set -e

GIT_USERNAME=$1
GIT_EMAIL=$2
GIT_COMMIT_SHA=$3
BLOG_DIRECTORY=$4

if [ -z $GIT_USERNAME ]; then
    echo "Please provide a username to use as git deployer"
    exit 1
elif [ -z $GIT_EMAIL ]; then
    echo "Please provide an email to use as git deployer"
    exit 1
elif [ -z $GIT_COMMIT_SHA ]; then
    echo "Please provide an commit use as deploy message"
    exit 1
elif [ -z $BLOG_DIRECTORY ]; then
    echo "Please provide the directory where blog is stored"
    exit 1
fi

if [ -f "./node_modules/.bin/gh-pages" ]; then
    echo "gh-pages already installed!"
else
    npm install --silent gh-pages@2.0.1
fi

git config user.email ${GIT_EMAIL}
git config user.name ${GIT_USERNAME}

GIT_COMMIT_MESSAGE=$(git log --format=oneline -n 1 $GIT_COMMIT_SHA)

./node_modules/.bin/gh-pages \
    --message "Auto-generated - ${GIT_COMMIT_MESSAGE} [ci skip]" \
    --dist ${BLOG_DIRECTORY}
