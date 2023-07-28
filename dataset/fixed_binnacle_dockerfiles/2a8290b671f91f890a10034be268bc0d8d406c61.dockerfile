#   Dockerfile for purkinje development
FROM ubuntu:16.04
MAINTAINER Bernhard Biskup <bbiskup@gmx.de>
#   Install dependencies
RUN echo 'Running installation'
WORKDIR /code
ENV DEBIAN_FRONTEND="noninteractive"
ENV NODE_DIR="node-v6.2.0-linux-x64"
ENV NODE_ARCHIVE="$NODE_DIR.tar.xz"
ENV PATH="/opt/node/bin:$PATH"
RUN apt-get update -q -y \
 && apt-get install --no-install-recommends wget=1.17.1-1ubuntu1.5 -y
RUN echo 1 > cache_bust.txt
#   Install Google Chrome APT repository
RUN wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -
RUN echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list
RUN apt-get update -q -y \
 && apt-get install --no-install-recommends dbus-x11=1.10.6-1ubuntu3.6 default-jre-headless=2:1.8-56ubuntu2 firefox=88.0+build2-0ubuntu0.16.04.1 gcc=4:5.3.1-1ubuntu1 git=1:2.7.4-0ubuntu1.10 google-chrome-stable libyaml-dev=0.1.6-3 make=4.1-6 python2.7=2.7.12-1ubuntu0~16.04.18 python2.7-dev=2.7.12-1ubuntu0~16.04.18 python3.5=3.5.2-2ubuntu0~16.04.13 python3.5-dev=3.5.2-2ubuntu0~16.04.13 software-properties-common=0.96.20.10 wget=1.17.1-1ubuntu1.5 xvfb=2:1.18.4-0ubuntu0.12 xz-utils=5.1.1alpha+20120614-2ubuntu2 -y \
 && rm -rf /var/lib/apt/lists/*
#   To avoid chrome waiting for gnome keyring
ENV DBUS_SESSION_BUS_ADDRESS="/dev/null"
RUN dpkg -r libfolks-eds25 gnome-keyring seahorse gcr evolution-data-server oneconf python-ubuntuone-storageprotocol ubuntu-sso-client python-ubuntu-sso-client pinentry-gnome3
#   TODO remove git dependency when removing bower
#   Install node.js; use most recent version to have access to latest features
WORKDIR /opt
RUN wget -q https://nodejs.org/dist/v6.2.0/$NODE_ARCHIVE \
 && tar xJf $NODE_ARCHIVE \
 && ln -s /opt/$NODE_DIR /opt/node \
 && rm $NODE_ARCHIVE
WORKDIR /code
RUN node --version
RUN npm --version
#   make npm less noisy
RUN npm config set loglevel warn
RUN python2.7 --version
RUN ln -sf /usr/bin/python2.7 /usr/bin/python
#   Ubuntu's python-pip throws exception with requests lib
#   see https://bugs.launchpad.net/ubuntu/+source/python-pip/+bug/1306991
RUN wget -q https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
COPY package.json /code/package.json
COPY bower.json /code/bower.json
COPY .jshintrc /code/.jshintrc
COPY .bowerrc /code/.bowerrc
RUN npm install
#   Set up Chrome webdriver for Protractor
RUN echo 1 > cache_bust.txt
RUN ./node_modules/protractor/bin/webdriver-manager update --standalone
RUN npm install bower@1.8.14 -g
RUN bower --allow-root --quiet install -F 2>&1 > bower.log
COPY requirements.txt /code/requirements.txt
COPY dev-requirements.txt /code/dev-requirements.txt
#   Python
RUN pip install $HOME/.pip-cache -q --upgrade -r dev-requirements.txt --cache-dir
#   Avoid Flask freezing
#   watchdog not compatible with gevent
#   see https://github.com/gorakhargosh/watchdog/issues/306
RUN pip uninstall -y watchdog
RUN echo "Installed Python packages:"
RUN pip freeze
COPY pytest.ini /code/pytest.ini
COPY tox.ini /code/tox.ini
COPY MANIFEST.in /code/MANIFEST.in
COPY setup.py /code/setup.py
COPY Makefile /code/Makefile
COPY purkinje /code/purkinje
COPY ./docker/purkinje*.yml /code/
COPY README.rst README.rst
COPY CHANGES.rst CHANGES.rst
RUN pip install -e .
RUN python setup.py sdist
ENV NODE_ARCHIVE="\"
ENV NODE_DIR="\"
COPY docker/purkinje.docker.yml purkinje.yml
ENTRYPOINT ["purkinje", "-c", "purkinje.yml"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
