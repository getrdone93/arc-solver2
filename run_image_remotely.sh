#!/bin/bash

set -e

if [[ "$#" -ne 3 ]];
then
  echo "usage: $0 [image-name] [tag] [algo]"
  exit
fi

IMAGE_NAME="$1"
TAG="$2"
ALGO="$3"
REMOTE_IMAGE="trever:5000/$IMAGE_NAME:$TAG"

ssh trever.anderson@trever "docker run -d -v /home/trever.anderson/git/personal/ARC/data/:/mnt/data/ $REMOTE_IMAGE $ALGO"