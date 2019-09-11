#!/bin/bash

set -e

EMACS_VERSION=$1
EMACS_INSTALL_DIRECTORY=$2

if [ -z $EMACS_VERSION ]; then
    echo "Please provide a Emacs version to install."
    exit 1
elif [ -z $EMACS_INSTALL_DIRECTORY ]; then
    echo "Please provide a directory to install Emacs."
    exit 1
fi

function download_emacs
{
    EMACS_URL_DOWNLOAD="https://github.com/npostavs/emacs-travis/releases/download/bins/emacs-bin-${EMACS_VERSION}.tar.gz"
    curl -fsSkL --retry 9 --retry-delay 9 -O ${EMACS_URL_DOWNLOAD}
    tar -xvf emacs-bin-${EMACS_VERSION}.tar.gz -C ${EMACS_INSTALL_DIRECTORY}
    chmod +x ${EMACS_INSTALL_DIRECTORY}/tmp/emacs/bin/emacs-${EMACS_VERSION}
}

export PATH=${EMACS_INSTALL_DIRECTORY}/tmp/emacs/bin:${PATH}

if [ -d "${EMACS_INSTALL_DIRECTORY}/tmp/emacs/bin" ]; then
    echo "Emacs already installed!"
else
    echo "Installing Emacs v$EMACS_VERSION into $EMACS_INSTALL_DIRECTORY"
    download_emacs
fi

emacs --version
