FROM ubuntu:xenial
LABEL Description="SPIRE Demo: Blog"
LABEL vendor="scytale.io"
LABEL version="0.1.0"
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends openssl=1.0.2g-1ubuntu4.20 -y )
#  ====================
#   Setup user accounts
#  --------------------
RUN addgroup --gid 1901 spire \
 && adduser --uid 1101 --disabled-password --shell /bin/bash --ingroup spire spire
RUN addgroup --gid 1910 blog \
 && adduser --uid 1111 --disabled-password --shell /bin/bash --ingroup blog blog
WORKDIR /home/aws
COPY conf /cmd/spire-agent/.conf
COPY pconf /plugin/agent/.conf
COPY .artifacts/artifacts.tgz /
COPY sidecar_config.hcl /sidecar/
RUN tar --directory / -xvzf /artifacts.tgz
ENV SPIRE_PLUGIN_CONFIG_DIR="/pconf"
#  ====================
#   Setup Python and FlaskBB
#  --------------------
RUN (apt-get update ;apt-get install --no-install-recommends make=4.1-6 wget=1.17.1-1ubuntu1.5 build-essential=12.1ubuntu2 python=2.7.12-1~16.04 python-dev=2.7.12-1~16.04 libmysqlclient-dev=5.7.33-0ubuntu0.16.04.1 git=1:2.7.4-0ubuntu1.10 -y )
RUN cd /home/ \
 && wget https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
RUN cd /home/ \
 && git clone https://github.com/sh4nks/flaskbb.git \
 && cd flaskbb \
 && pip install -r requirements.txt
COPY flaskbb_requirements.txt /home/flaskbb/
COPY flaskbb.cfg /home/flaskbb/
RUN cd /home/flaskbb \
 && pip install -r flaskbb_requirements.txt
EXPOSE 8080/tcp
#  ====================
#   Setup Ghostunnel
#  --------------------
RUN mkdir /home/ghostunnel
COPY /keys/ /keys/
COPY ghostunnel_client.sh /home/ghostunnel/
COPY .artifacts/ghostunnel /home/ghostunnel/
#  ====================
#   Vault setup: Eventually move to its own container
#  --------------------
#   Create a vault user and group first so the IDs are the same
RUN addgroup --gid 120 vault \
 && adduser --uid 1010 --disabled-password --shell /bin/bash --ingroup vault vault
RUN mkdir -p /vault/logs \
 && mkdir -p /vault/
#   TODO
#   Expose the logs directory
VOLUME /vault/logs
#   Expose the file directory
VOLUME /vault/file
#  ================
#   Final setup
#  ----------------
WORKDIR /cmd/spire-agent/
#   CMD ./spire-agent run
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
