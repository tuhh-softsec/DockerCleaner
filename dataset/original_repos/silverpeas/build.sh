#!/usr/bin/env bash

if [ $# -ne 0 ]; then
  version="$1"
  git checkout ${version}
else
  version=`grep 'ENV SILVERPEAS_VERSION' Dockerfile | cut -d '=' -f 2`
fi

echo "Build a docker image for Silverpeas ${version}"
sleep 1
docker build -t silverpeas:${version} .

