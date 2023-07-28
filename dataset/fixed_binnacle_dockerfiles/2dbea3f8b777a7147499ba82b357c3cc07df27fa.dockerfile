#   This Docker image encapsulates the File Scanning Framework (FSF) by 
#   Emerson Electric Company from https://github.com/EmersonElectricCo/fsf 
#
#   To run this image after installing Docker using a standalone instance, use a command like 
#   the following, replacing “~/fsf-workdir" with the path to the location of your FSF
#   working directory:
#
#   sudo docker run --rm -it -v ~/fsf-workdir:/home/nonroot/workdir wzod/fsf
#
#   To run this image using a networked instance, use a command like this after installing
#   FSF on the host system:
#
#   sudo docker run --rm -it -p 5800:5800 -v ~/fsf-workdir:/home/nonroot/workdir wzod/fsf
#
#   Before running FSF, create the  ~/fsf-workdir and make it world-accessible
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
FROM ubuntu:16.04
MAINTAINER Zod (@wzod)
ENV DEBIAN_FRONTEND="noninteractive"
USER root
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y \
 && apt-add-repository -y multiverse \
 && apt-get update -qq \
 && apt-get install --no-install-recommends autoconf=2.69-9 automake=1:1.15-4ubuntu1 build-essential=12.1ubuntu2 cabextract=1.6-1 dh-autoreconf=11 git=1:2.7.4-0ubuntu1.10 jq=1.5+dfsg-1ubuntu0.1 libffi-dev=3.2.1-4 libfuzzy-dev=2.13-2 libpython2.7-stdlib=2.7.12-1ubuntu0~16.04.18 libssl-dev=1.0.2g-1ubuntu4.20 libtool=2.4.6-0.1 make=4.1-6 net-tools=1.60-26ubuntu1 python-dev=2.7.12-1~16.04 python-minimal=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 python-setuptools=20.7.0-1 ssdeep=2.13-2 unrar=1:5.3.2-1+deb9u1build0.16.04.1 unzip=6.0-20ubuntu1.1 upx-ucl=3.91-1 vim=2:7.4.1689-3ubuntu1.5 wget=1.17.1-1ubuntu1.5 -y --fix-missing \
 && pip install setuptools==67.6.1 --upgrade
#   Retrieve current version of Yara via wget, verify known good hash and install Yara
RUN
#   Install additional dependencies
RUN pip install czipfile==1.0.0 hachoir-parser==null hachoir-core==null hachoir-regex==null hachoir-metadata==null hachoir-subfile==null ConcurrentLogHandler==0.9.1 pypdf2==3.0.1 xmltodict==0.13.0 rarfile==4.0 pylzma==0.5.0 oletools==0.60.1 pyasn1_modules==0.2.8 pyasn1==0.4.8 pyelftools==0.29 javatools==1.5.0 requests==2.28.2 git+https://github.com/aaronst/macholibre.git \
 && BUILD_LIB=1 pip install ssdeep
#   Add nonroot user, clone repo and setup environment
RUN groupadd -r nonroot \
 && useradd -r -g nonroot -d /home/nonroot -s /sbin/nologin -c "Nonroot User" nonroot \
 && mkdir /home/nonroot \
 && chown -R nonroot:nonroot /home/nonroot \
 && echo "/usr/local/lib" >> /etc/ld.so.conf.d/yara.conf
USER nonroot
RUN mkdir -pv /home/nonroot/workdir \
 && cd /home/nonroot \
 && git clone https://github.com/EmersonElectricCo/fsf.git \
 && cd fsf/ \
 && sed -i 's/\/FULL\/PATH\/TO/\/home\/nonroot/' fsf-server/conf/config.py \
 && sed -i "/^SCANNER\_CONFIG/ s/\/tmp/\/home\/nonroot\/workdir/" fsf-server/conf/config.py
USER root
RUN ldconfig \
 && ln -f -s /home/nonroot/fsf/fsf-server/main.py /usr/local/bin/ \
 && ln -f -s /home/nonroot/fsf/fsf-client/fsf_client.py /usr/local/bin/ \
 && apt-get remove -y --purge automake build-essential libtool \
 && apt-get autoremove -y --purge \
 && apt-get clean -y \
 && rm -rf /var/lib/apt/lists/*
USER nonroot
ENV HOME="/home/nonroot"
ENV USER="nonroot"
WORKDIR /home/nonroot/workdir
ENTRYPOINT sed -i "/^SERVER_CONFIG/ s/127\.0\.0\.1/$( hostname -i ;)/" /home/nonroot/fsf/fsf-client/conf/config.py \
 && main.py start \
 && printf "\n\n" \
 && echo "<----->" \
 && echo "FSF server daemonized!" \
 && echo "<----->" \
 && printf "\n" \
 && echo "Invoke fsf_client.py by giving it a file as an argument:" \
 && printf "\n" \
 && echo "fsf_client.py <file>" \
 && printf "\n" \
 && echo "Alternatively, Invoke fsf_client.py by giving it a file as an argument and pass to jq so you can interact extensively with the JSON output:" \
 && printf "\n" \
 && echo "fsf_client.py <file> | jq -C . | less -r" \
 && printf "\n" \
 && echo "To access all of the subobjects that are recursively processed, simply add --full when invoking fsf_client.py:" \
 && printf "\n" \
 && echo "fsf_client.py <file> --full" \
 && printf "\n" \
 && /bin/bash
# Please add your HEALTHCHECK here!!!
