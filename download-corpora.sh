#!/bin/sh

# This script downloads the corpora required to run certain tests and
# benchmarks.

BASE_DIR="corpora"
CORPORA="cantrbry artificl large"

# $1 = list of files to download from http://corpus.canterbury.ac.nz/resources/
#      without suffix
# $2 = directory to download and unzip (relative)
download_and_unzip() {
    # Make the base directory if it does not exist
    if test ! -d $2
    then
        echo "mkdir -p $2"
        mkdir -p $2
    fi

    for file in $1
    do
        # Remove existing files
        if test -d "$2/$file"
        then
            echo "rm -rf $2/$file"
            rm -rf "$2/$file"
        fi

        # Downloading tar
        if test ! -e "$2/${file}.tar.gz"
        then
            # Download
            echo "Downloading ${file}.tar.gz"
            wget -P $2 "http://corpus.canterbury.ac.nz/resources/${file}.tar.gz"
        else
            echo "${file}.tar.gz already exists, skipping"
        fi

        # Make directory
        echo "mkdir -p $2/$file"
        mkdir -p "$2/$file"

        # Extract files
        echo "Extracting $2/${file}.tar.gz"
        tar xf "$2/${file}.tar.gz" -C "$2/$file"
    done
}

download_and_unzip "$CORPORA" $BASE_DIR
