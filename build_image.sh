#!/bin/bash

set -e

if [[ "$#" -ne 2 ]];
then
  echo "usage: $0 [image-name] [tag]"
  exit
fi

lein uberjar
docker build . -t "$1:$2"