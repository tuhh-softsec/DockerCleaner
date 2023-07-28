#   This Docker image encapsulates the Laika BOSS: Object Scanning System by 
#   Lockheed Martin Corporation from https://github.com/lmco/laikaboss 
#
#   To run this image after installing Docker using a standalone instance, use a command like 
#   the following, replacing “~/laikaboss-workdir" with the path to the location of your 
#   Laika BOSS working directory:
#
#   sudo docker run --rm -it -v ~/laikaboss-workdir:/home/nonroot/workdir wzod/laikaboss
#
#   To run this image using a networked instance, use a command like this:
#
#   sudo docker run --rm -it -p 5558:5558 -v ~/laikaboss-workdir:/home/nonroot/workdir wzod/laikaboss
#
#   Before running Laika BOSS, create the  ~/laikaboss-workdir and make it world-accessible
#   (“chmod a+xwr").
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#   
#       http://www.apache.org/licenses/LICENSE-2.0
#   
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
FROM ubuntu:14.04
MAINTAINER Zod (@wzod)
ENV DEBIAN_FRONTEND="noninteractive"
USER root
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.92.37.8 -y \
 && apt-add-repository -y multiverse \
 && apt-get update -qq \
 && apt-get install --no-install-recommends automake=1:1.14.1-2ubuntu1 build-essential=11.6ubuntu6 git=1:1.9.1-1ubuntu0.10 jq=1.3-1.1ubuntu1.1 libfuzzy-dev=2.7-2ubuntu1 libimage-exiftool-perl=9.46-1 liblzma5=5.1.1alpha+20120614-2ubuntu2 libssl-dev=1.0.1f-1ubuntu2.27 libtool=2.4.2-1.7ubuntu1 libzmq3=4.0.4+dfsg-2ubuntu0.1 make=3.81-8.2ubuntu3 python-cffi=0.8.2-0ubuntu1 python-dev=2.7.5-5ubuntu3 python-gevent=1.0-1ubuntu1.1 python-ipy=1:0.81-1 python-m2crypto=0.21.1-3ubuntu5 python-pexpect=3.1-1ubuntu0.1 python-pip=1.5.4-1ubuntu4 python-progressbar=2.3-1 python-pyclamd=0.2.2-1 python-zmq=14.0.1-1build2 unrar=1:5.0.10-1ubuntu0.14.04.1 unzip=6.0-9ubuntu1.5 wget=1.15-1ubuntu1.14.04.5 -y \
 && pip install setuptools==67.6.1 --upgrade
#   Retrieve current version of Yara via wget, verify known good hash and install Yara
RUN
#   Install additional dependencies
RUN cd /tmp \
 && pip install fluent-logger==0.10.0 future==0.18.3 interruptingcow==0.8 javatools==1.5.0 msgpack-python==0.5.6 olefile==0.46 pylzma==0.5.0 py-unrar2==0.99.6 ssdeep==3.4
#   Add nonroot user, clone repo and setup environment
RUN groupadd -r nonroot \
 && useradd -r -g nonroot -d /home/nonroot -s /sbin/nologin -c "Nonroot User" nonroot \
 && mkdir /home/nonroot \
 && chown -R nonroot:nonroot /home/nonroot
#   Clone Laika BOSS from GitHub as nonroot user
USER nonroot
RUN cd /home/nonroot \
 && git clone https://github.com/lmco/laikaboss.git
#   Run setup script to install Laika BOSS framework, client library, modules and associated scripts (laika.py, laikad.py, cloudscan.py)
USER root
RUN cd /home/nonroot/laikaboss/ \
 && python setup.py build \
 && python setup.py install
#   Clean up and run ldconfig
RUN ldconfig \
 && apt-get remove -y --purge automake build-essential libtool \
 && apt-get autoremove -y --purge \
 && apt-get clean -y \
 && rm -rf /var/lib/apt/lists/*
USER nonroot
ENV HOME="/home/nonroot"
ENV USER="nonroot"
WORKDIR /home/nonroot/workdir
ENTRYPOINT echo "To run the standalone scanner, execute laika.py against a file like so:" \
 && printf "\n""laika.py <filename> | jq -C . | less -r" \
 && printf "\n\n""To run the networked instance, first execute laikad.py and use cloudscan against like so:" \
 && printf "\n\n""laikad.py &" \
 && printf "\n\n""cloudscan.py <filename> | jq -C . | less -r" \
 && printf "\n\n" \
 && /bin/bash
# Please add your HEALTHCHECK here!!!
