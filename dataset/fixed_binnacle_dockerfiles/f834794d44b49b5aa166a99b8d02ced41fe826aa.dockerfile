FROM ubuntu:xenial
#   Lovingly borrowed and modified from https://github.com/StackStorm/st2-dockerfiles/blob/master/base/Dockerfile
ARG DEBIAN_FRONTEND=noninteractive
ARG ST2_VERSION=2.10.1
RUN : "${ST2_VERSION:?Docker build argument needs to be set and non-empty.}"
ENV container="docker"
ENV ENV="/etc/skel/.profile"
ENV TERM="xterm"
#   Generate and set locale to UTF-8
RUN apt-get update -qq \
 && (apt-get update ;apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 locales=2.23-0ubuntu11.3 -y ) \
 && rm -rf /var/lib/apt/lists/* \
 && locale-gen en_US.UTF-8 \
 && update-locale LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8
#   The LC_ALL variable must be defined after executing update-local
ENV LANG="en_US.UTF-8" \
    LANGUAGE="en_US:en" \
    LC_ALL="en_US.UTF-8"
#  ####################################################################################################
#  # External Deps               
#  ####################################################################################################
RUN mkdir -p /data/db
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends gnupg-curl=1.4.20-1ubuntu3.3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 wget=1.17.1-1ubuntu1.5 -y )
#   Add key and repo for the latest stable MongoDB (3.4)
RUN wget -qO - https://www.mongodb.org/static/pgp/server-3.4.asc | apt-key add -
RUN echo "deb http://repo.mongodb.org/apt/ubuntu xenial/mongodb-org/3.4 multiverse" > /etc/apt/sources.list.d/mongodb-org-3.4.list
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends crudini=0.7-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends mongodb-org -y )
RUN (apt-get update ;apt-get install --no-install-recommends rabbitmq-server=3.5.7-1ubuntu0.16.04.4 -y )
#  ####################################################################################################
#  # Install StackStorm                 
#  ####################################################################################################
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
#   Install StackStorm, but without UI
RUN curl -sf https://packagecloud.io/install/repositories/StackStorm/stable/script.deb.sh | bash \
 && (apt-get update ;apt-get install --no-install-recommends st2=${ST2_VERSION}-* -y ) \
 && rm -f /etc/apt/sources.list.d/StackStorm_*.list
COPY htpasswd /etc/st2/htpasswd
RUN echo "stanley:stanley" | chpasswd
COPY start_st2_services.sh /
#  ####################################################################################################
#  # Additional installations / configuration             
#  ####################################################################################################
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends openssh-server=1:7.2p2-4ubuntu2.10 python=2.7.12-1~16.04 git=1:2.7.4-0ubuntu1.10 vim=2:7.4.1689-3ubuntu1.5 screen=4.3.1-2ubuntu0.1 -y )
COPY napalm.yaml /opt/stackstorm/configs
RUN cp -r /usr/share/doc/st2/examples/ /opt/stackstorm/packs/
ARG CACHEBUST=1
RUN screen -d -m /start_st2_services.sh \
 && sleep 15 \
 && st2ctl reload --register-all \
 && st2 run packs.setup_virtualenv packs=examples \
 && st2 pack remove napalm \
 && st2 pack install https://github.com/nre-learning/stackstorm-napalm.git
RUN /opt/stackstorm/virtualenvs/napalm/bin/pip install ncclient==0.6.0
ARG CACHEBUST=0
#  ####################################################################################################
#  # Security options (copied from Utility)      
#  ####################################################################################################
RUN mkdir /var/run/sshd
#   Antidote user
RUN mkdir -p /home/antidote
RUN useradd antidote -p antidotepassword
RUN mkdir -p /home/antidote/.st2/
COPY st2config /home/antidote/.st2/config
RUN chown antidote:antidote /home/antidote /home/antidote/.st2 /home/antidote/.st2/config
RUN chsh antidote --shell=/bin/bash
RUN echo 'antidote:antidotepassword' | chpasswd
RUN echo 'root:$(uuidgen)' | chpasswd
#   Adjust MOTD
RUN rm -f /etc/update-motd.d/*
RUN rm -f /etc/legal
COPY .welcome.sh /etc/update-motd.d/00-antidote-motd
RUN chmod +x /etc/update-motd.d/00-antidote-motd
#   Disable root Login
RUN sed -i 's/PermitRootLogin prohibit-password/PermitRootLogin no/' /etc/ssh/sshd_config
RUN sed -i 's/PermitRootLogin yes/PermitRootLogin no/' /etc/ssh/sshd_config
#   SSH login fix. Otherwise user is kicked off after login
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd
#   Disable su for everyone not in the wheel group (no one is in the wheel group)
RUN echo "auth required pam_wheel.so use_uid" >> /etc/pam.d/su
ENV NOTVISIBLE="\"in users profile\""
RUN echo "export VISIBLE=now" >> /etc/profile
CMD /start_st2_services.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
