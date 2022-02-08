#!/bin/bash

set -e

if [[ "$#" -lt 2 || "$#" -gt 3 ]];
then
  #{} means optional
  echo "usage: $0 [image-name] ['algo1 algo2'] {tag}"
  exit
fi

IMAGE_NAME="$1"
ALGO=$2

BRANCH=$(git branch | grep '*' | cut -d' ' -f 2)
COMMIT=$(git rev-parse --short HEAD)
TAG="$BRANCH-$COMMIT"
if [[ ! -z $3 ]]; then
  TAG="$3"
fi


echo "build and run for $TAG"

./start_registry.sh
./build_image.sh "$IMAGE_NAME" "$TAG"
./push_image.sh "$IMAGE_NAME" "$TAG"
for a in $ALGO; do
  ./run_image_remotely.sh "$IMAGE_NAME" "$TAG" "$a"
done
