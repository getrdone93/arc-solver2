#!/bin/bash

set -e

if [[ "$#" -ne 2 ]];
then
  echo "usage: $0 [image-name] [tag]"
  exit
fi

IMAGE_NAME="$1"
TAG="$2"
LOCAL_IMAGE="$IMAGE_NAME:$TAG"
REMOTE_IMAGE="trever:5000/$LOCAL_IMAGE"

docker tag "$LOCAL_IMAGE" "$REMOTE_IMAGE"
docker push "$REMOTE_IMAGE"
