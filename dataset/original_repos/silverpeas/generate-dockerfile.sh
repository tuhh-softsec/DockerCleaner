#!/usr/bin/env bash

usage() {
  cat <<-EOL
Usage: build.sh SILVERPEAS_VERSION WILDFLY_VERSION
Generate a new Dockerfile for the specified Silverpeas and Wildfly versions.
The Dockerfile is generated from the template Dockerfile.template.

  SILVERPEAS_VERSION  the version of Silverpeas targeted by the Dockerfile to
                      generate.
  WILDFLY_VERSION     the version of Wildfly to which the targeted Silverpeas 
                      belongs (without the end term .Final).
EOL
}
  
if [ $# -ne 2 ]; then
  usage
else 
  silverpeas_version=$1
  wildfly_version=$2

  sed -e "s/TARGET_SILVERPEAS_VERSION/${silverpeas_version}/g" Dockerfile.template > Dockerfile
  sed -i -e "s/TARGET_WILDFLY_VERSION/${wildfly_version}/g" Dockerfile
  sed -i -e "s/build=.\+/build=1/g" Dockerfile
fi
