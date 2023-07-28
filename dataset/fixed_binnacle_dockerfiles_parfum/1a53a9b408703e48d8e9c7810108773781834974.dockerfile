ARG PYTHON_VERSION
FROM python:$PYTHON_VERSION
#  install node
RUN apt-get update ; curl -sL https://deb.nodesource.com/setup_11.x | bash - ; apt-get install --no-install-recommends nodejs -y
#  install rsync
RUN apt-get install --no-install-recommends rsync -y
#  install yarn
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - ; echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list ; apt-get update \
 && apt-get install --no-install-recommends yarn
#  install docker-compose
RUN curl -L "https://github.com/docker/compose/releases/download/1.24.0/docker-compose-$( uname -s ;)-$( uname -m ;)" -o /usr/local/bin/docker-compose ; chmod +x /usr/local/bin/docker-compose
#  install docker
RUN apt-get install --no-install-recommends apt-transport-https ca-certificates curl gnupg2 software-properties-common -y; curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add - ; add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/debian $( lsb_release -cs ;) stable" ; apt-get update ; apt-get install --no-install-recommends docker-ce -y
#  install java
#  We need to use java 8 because https://stackoverflow.com/questions/53583199/pyspark-error-unsupported-class-file-major-version-55
# #################################################################################################
#  https://github.com/docker-library/openjdk/blob/c3023e4da10d10e9c9775eabe2d7baac146e7ae1/8/jdk/slim/Dockerfile
#  A few reasons for installing distribution-provided OpenJDK:
#
#   1. Oracle.  Licensing prevents us from redistributing the official JDK.
#
#   2. Compiling OpenJDK also requires the JDK to be installed, and it gets
#      really hairy.
#
#      For some sample build times, see Debian's buildd logs:
#        https://buildd.debian.org/status/logs.php?pkg=openjdk-8
RUN apt-get install --no-install-recommends bzip2 unzip xz-utils -y \
 && rm -rf /var/lib/apt/lists/*
#  Default to UTF-8 file.encoding
ENV LANG="C.UTF-8"
#  add a simple script that can auto-detect the appropriate JAVA_HOME value
#  based on whether the JDK or only the JRE is installed
RUN { echo '#!/bin/sh' ;echo 'set -e' ;echo ;echo 'dirname "$(dirname "$(readlink -f "$(which javac || which java)")")"' ; } > /usr/local/bin/docker-java-home \
 && chmod +x /usr/local/bin/docker-java-home
#  do some fancy footwork to create a JAVA_HOME that's cross-architecture-safe
RUN ln -svT "/usr/lib/jvm/java-8-openjdk-$( dpkg --print-architecture ;)" /docker-java-home
ENV JAVA_HOME="/docker-java-home"
#  Updated from base image
ENV JAVA_VERSION="8u212"
ENV JAVA_DEBIAN_VERSION="8u212-b01-1~deb9u1"
RUN set -ex ; if [ ! -d /usr/share/man/man1 ] ; then mkdir -p /usr/share/man/man1 ; fi ; apt-get update ; apt-get install --no-install-recommends openjdk-8-jre-headless="$JAVA_DEBIAN_VERSION" -y; apt-get install --no-install-recommends openjdk-8-jdk-headless="$JAVA_DEBIAN_VERSION" -y; rm -rf /var/lib/apt/lists/* ; [ "$( readlink -f "$JAVA_HOME" ;)" = "$( docker-java-home ;)" ] ; update-alternatives --get-selections | awk -v home="$( readlink -f "$JAVA_HOME" ;)" 'index($3, home) == 1 { $2 = "manual"; print | "update-alternatives --set-selections" }' ; update-alternatives --query java | grep -q 'Status: manual'
# #################################################################################################
#  Install sbt
ARG SBT_VERSION=1.2.8
RUN curl -L -o sbt-$SBT_VERSION.deb https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb \
 && dpkg -i sbt-$SBT_VERSION.deb \
 && rm sbt-$SBT_VERSION.deb \
 && apt-get update \
 && apt-get install --no-install-recommends sbt \
 && sbt sbtVersion
# #################################################################################################
#
#  pip install all the downstream deps to speed up our CI jobs
#
#  to update (from root):
#
#  create a new virtual env
#  make install_dev_python_modules
#  pip freeze --exclude-editable > .buildkite/images/integration/snapshot-reqs.txt
RUN pip install pyspark==2.4.0 --no-cache-dir
#  This instigates some package downloads required by the airline-demo
COPY trigger_maven.py .
RUN python trigger_maven.py
#  Pre-load jars for scala_modules by running a compile
COPY scala_modules scala_modules
RUN cd scala_modules \
 && make compile
#  Postgres (libpq) required for the airline demo
RUN pip install tox
RUN apt-get update \
 && apt-get install --no-install-recommends libpq-dev -y
ARG PYTHON_MAJOR_VERSION
#  These are at the bottom and because they are the most likely
#  to change frequently
COPY snapshot-reqs-$PYTHON_MAJOR_VERSION.txt /tmp/snapshot-reqs.txt
RUN pip install pip --upgrade ; pip install -r /tmp/snapshot-reqs.txt
