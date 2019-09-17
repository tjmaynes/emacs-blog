#!/bin/bash

set -e

GIT_USERNAME=$1
GIT_EMAIL=$2
GIT_COMMIT_SHA=$3
TARGET_BRANCH=$4
BLOG_DIRECTORY=$5

if [ -z $GIT_USERNAME ]; then
    echo "Please provide a username to use as git deployer"
    exit 1
elif [ -z $GIT_EMAIL ]; then
    echo "Please provide an email to use as git deployer"
    exit 1
elif [ -z $GIT_COMMIT_SHA ]; then
    echo "Please provide an commit use as deploy message"
    exit 1
elif [ -z $TARGET_BRANCH ]; then
    echo "Please provide a target branch to deploy to"
    exit 1
elif [ -z $BLOG_DIRECTORY ]; then
    echo "Please provide the directory where blog is stored"
    exit 1
fi

REPO=git@github.com:tjmaynes/blog.git
CURRENT_BRANCH=$(git branch | grep \* | cut -d ' ' -f2)
GIT_COMMIT_MESSAGE=$(git log --format=oneline -n 1 $GIT_COMMIT_SHA)

if ! [ $CURRENT_BRANCH == "master" ]; then
    echo "Will not deploy from non-master branch"
    exit 1
fi

if ! [ $(git ls-remote --heads $REPO $TARGET_BRANCH | wc -l) -ge 1 ]; then
    echo "Creating remote branch: ${TARGET_BRANCH}"
    git checkout -b ${TARGET_BRANCH}
    git commit --allow-empty -m "first commit"
    git push origin ${TARGET_BRANCH}
    git checkout master
fi

if [ -d gh-pages-temp ]; then rm -rf gh-pages-temp; fi

git config user.email ${GIT_EMAIL}
git config user.name ${GIT_USERNAME}

# Clone Target Branch and remove content
git clone \
    --single-branch --branch ${TARGET_BRANCH} \
    $REPO gh-pages-temp && \
    cd gh-pages-temp && \
    git rm -rf . > /dev/null 2>&1 && \
    cd ../

cp -a ${BLOG_DIRECTORY}/. gh-pages-temp

# Deploy changes
cd gh-pages-temp && \
    git add . && \
    git commit -m  "Auto-generated - ${GIT_COMMIT_MESSAGE} [ci skip]" && \
    git push origin ${TARGET_BRANCH} && \
    cd ../

# Cleanup
rm -rf gh-pages-temp
