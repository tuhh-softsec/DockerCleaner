FROM ubuntu:14.04
MAINTAINER Open State Foundation <developers@openstate.eu>
#   Use bash as default shell
RUN rm /bin/sh \
 && ln -s /bin/bash /bin/sh
#   Add multiverse to sources
RUN echo 'deb http://archive.ubuntu.com/ubuntu/ trusty multiverse' >> etc/apt/sources.list
#   Set Dutch locale, needed to parse Dutch time data
RUN locale-gen nl_NL.UTF-8
#  Set Timezone
RUN echo "Europe/Amsterdam" > /etc/timezone \
 && dpkg-reconfigure -f noninteractive tzdata
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends python-dev=2.7.5-5ubuntu3 python-setuptools=3.3-1ubuntu2 python-software-properties=0.92.37.8 openjdk-7-jre-headless=7u211-2.6.17-0ubuntu0.1 wget=1.15-1ubuntu1.14.04.5 curl=7.35.0-1ubuntu2.20 poppler-utils=0.24.5-2ubuntu4.17 software-properties-common=0.92.37.8 gettext=0.18.3.1-1ubuntu3.1 git=1:1.9.1-1ubuntu0.10 dnsutils=1:9.9.5.dfsg-3ubuntu0.19 vim=2:7.4.052-1ubuntu3.1 -y )
RUN add-apt-repository ppa:mc3man/trusty-media \
 && : \
 && apt-get dist-upgrade -y
#   according to http://www.monblocnotes.com/node/2057
RUN sed -i 's/exit 101/exit 0/' /usr/sbin/policy-rc.d
RUN (apt-get update ;apt-get install --no-install-recommends redis-server=2:2.8.4-2ubuntu0.2 -y )
RUN service redis-server start
#   Install elasticsearch
ENV ES_VERSION="1.4.2"
RUN wget https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-${ES_VERSION}.deb
RUN dpkg -i elasticsearch-${ES_VERSION}.deb > /dev/null
RUN service elasticsearch start
RUN rm elasticsearch-${ES_VERSION}.deb
#   Install elasticsearch head plugin
RUN /usr/share/elasticsearch/bin/plugin --install mobz/elasticsearch-head
RUN (apt-get update ;apt-get install --no-install-recommends make=3.81-8.2ubuntu3 libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 libxslt1-dev=1.1.28-2ubuntu0.2 libssl-dev=1.0.1f-1ubuntu2.27 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libtiff4-dev=4.0.3-7ubuntu0.11 libjpeg8-dev=8c-2ubuntu8 liblcms2-dev=2.5-0ubuntu4.2 python-dev=2.7.5-5ubuntu3 python-setuptools=3.3-1ubuntu2 python-virtualenv=1.11.4-1ubuntu1 git=1:1.9.1-1ubuntu0.10 supervisor=3.0b2-1ubuntu0.1 vim=2:7.4.052-1ubuntu3.1 -y )
RUN easy_install pip
#  #### Install dependencies for pyav #####
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libfaac-dev=1.28-6 libgpac-dev=0.5.0+svn4288~dfsg1-4ubuntu1 checkinstall=1.6.2-4ubuntu1 libmp3lame-dev=3.99.5+repack1-3ubuntu1 libopencore-amrnb-dev=0.1.3-2ubuntu1 libopencore-amrwb-dev=0.1.3-2ubuntu1 librtmp-dev=2.4+20121230.gitdf6c518-1ubuntu0.1 libtheora-dev=1.1.1+dfsg.1-3.2 libvorbis-dev=1.3.2-1.3ubuntu1.2 libx264-dev=2:0.142.2389+git956c8d8-2 libfdk-aac-dev=0.1.2-1 libvpx-dev=1.3.0-2 libxvidcore-dev=2:1.3.2-9ubuntu1 pkg-config=0.26-1ubuntu4 yasm=1.2.0-1ubuntu1 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 libavformat-dev=6:9.20-0ubuntu0.14.04.1 libavcodec-dev=6:9.20-0ubuntu0.14.04.1 libavdevice-dev=6:9.20-0ubuntu0.14.04.1 libavutil-dev=6:9.20-0ubuntu0.14.04.1 libswscale-dev=6:9.20-0ubuntu0.14.04.1 libavresample-dev=6:9.20-0ubuntu0.14.04.1 -y )
#   Temporarily use /tmp as workdir for the pyav dependencies
#   WORKDIR /tmp
RUN (apt-get update ;apt-get install --no-install-recommends ffmpeg -y )
#  #########
WORKDIR /opt/ocd
#   Create a virtualenv project
RUN echo 'ok'
RUN virtualenv -q /opt
RUN source ../bin/activate \
 && pip install pip==23.1 --upgrade
RUN echo "source /opt/bin/activate; cd /opt/ocd;" >> ~/.bashrc
#   Temporarily add files on the host to the container
#   as it contains files needed to finish the base installation
COPY . /opt/ocd
#   Install Python requirements
#   Install Python requirements
RUN source ../bin/activate \
 && pip install pycparser==2.13 \
 && pip install Cython==0.21.2 \
 && pip install -r requirements.txt
#   Initialize
RUN source ../bin/activate \
 && service elasticsearch start \
 && sleep 20 \
 && ./manage.py elasticsearch create_indexes es_mappings/ \
 && ./manage.py elasticsearch put_template
RUN (apt-get update ;apt-get install --no-install-recommends supervisor=3.0b2-1ubuntu0.1 -y )
#   Generate documentation
RUN source ../bin/activate \
 && cd docs \
 && make html
#   Delete all files again
RUN find . -delete
#   When the container is created or started run start.sh which starts
#   all required services and supervisor which starts celery and celerycam
CMD /opt/ocd/start.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
