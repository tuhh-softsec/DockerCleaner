#   VERSION 0.3.0
FROM ubuntu:16.04
MAINTAINER Shane Frasier <jeremy.frasier@trio.dhs.gov>
#  ##
#   Dependencies
#  ##
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update -qq \
 && apt-get install --no-install-recommends apt-utils=1.2.35 build-essential=12.1ubuntu2 curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 libc6-dev=2.23-0ubuntu11.3 libfontconfig1=2.11.94-0ubuntu1.1 libreadline-dev=6.3-8ubuntu2 libssl-dev=1.0.2g-1ubuntu4.20 libssl-doc=1.0.2g-1ubuntu4.20 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxslt1-dev=1.1.28-2.1ubuntu0.3 libyaml-dev=0.1.6-3 make=4.1-6 unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 autoconf=2.69-9 automake=1:1.15-4ubuntu1 bison=2:3.0.4.dfsg-1 gawk=1:4.1.3+dfsg-0.1 libffi-dev=3.2.1-4 libgdbm-dev=1.8.3-13.1 libncurses5-dev=6.0+20160213-1ubuntu1 libsqlite3-dev=3.11.0-1ubuntu1.5 libtool=2.4.6-0.1 pkg-config=0.29.1-0ubuntu1 sqlite3=3.11.0-1ubuntu1.5 libbz2-dev=1.0.6-8ubuntu0.2 llvm=1:3.8-33ubuntu3.1 libncursesw5-dev=6.0+20160213-1ubuntu1 nodejs=4.2.6~dfsg-1ubuntu4.2 npm=3.5.2-0ubuntu4 net-tools=1.60-26ubuntu1 fonts-liberation=1.07.4-1 libappindicator3-1=12.10.1+16.04.20170215-0ubuntu1 libasound2=1.1.0-0ubuntu1 libatk-bridge2.0-0=2.18.1-2ubuntu1 libgtk-3-0=3.18.9-1ubuntu3.3 libnspr4=2:4.13.1-0ubuntu0.16.04.1 libnss3=2:3.28.4-0ubuntu0.16.04.14 libxss1=1:1.2.2-1 libxtst6=2:1.2.2-1 lsb-release=9.20160110ubuntu0.2 xdg-utils=1.1.1-1ubuntu1.16.04.5 -qq --yes --no-install-suggests
RUN apt-get install --no-install-recommends locales=2.23-0ubuntu11.3 -qq --yes \
 && locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8" \
    LANGUAGE="en_US:en" \
    LC_ALL="en_US.UTF-8"
#  ##
#   Google Chrome
#  ##
RUN wget -q https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
 && dpkg -i google-chrome-stable_current_amd64.deb \
 && rm google-chrome-stable_current_amd64.deb
#   The third-parties scanner looks for an executable called chrome
RUN ln -s /usr/bin/google-chrome-stable /usr/bin/chrome
#  ##
#  # Python
#  ##
ENV PYENV_RELEASE="1.2.2" \
    PYENV_PYTHON_VERSION="3.6.4" \
    PYENV_ROOT="/opt/pyenv" \
    PYENV_REPO="https://github.com/pyenv/pyenv"
RUN wget ${PYENV_REPO}/archive/v${PYENV_RELEASE}.zip --no-verbose \
 && unzip v$PYENV_RELEASE.zip -d $PYENV_ROOT \
 && mv $PYENV_ROOT/pyenv-$PYENV_RELEASE/* $PYENV_ROOT/ \
 && rm -r $PYENV_ROOT/pyenv-$PYENV_RELEASE
#
#   Uncomment these lines if you just want to install python...
#
ENV PATH="$PYENV_ROOT/bin:$PYENV_ROOT/versions/${PYENV_PYTHON_VERSION}/bin:$PATH"
RUN echo 'eval "$(pyenv init -)"' >> /etc/profile \
 && eval "$( pyenv init - ;)" \
 && pyenv install $PYENV_PYTHON_VERSION \
 && pyenv local ${PYENV_PYTHON_VERSION}
#
#   ...uncomment these lines if you want to also debug python code in GDB
#
#   ENV PATH $PYENV_ROOT/bin:$PYENV_ROOT/versions/${PYENV_PYTHON_VERSION}-debug/bin:$PATH
#   RUN echo 'eval "$(pyenv init -)"' >> /etc/profile \
#       && eval "$(pyenv init -)" \
#       && pyenv install --debug --keep $PYENV_PYTHON_VERSION \
#       && pyenv local ${PYENV_PYTHON_VERSION}-debug
#   RUN ln -s /opt/pyenv/sources/${PYENV_PYTHON_VERSION}-debug/Python-${PYENV_PYTHON_VERSION}/python-gdb.py \
#       /opt/pyenv/versions/${PYENV_PYTHON_VERSION}-debug/bin/python3.6-gdb.py \
#       && ln -s /opt/pyenv/sources/${PYENV_PYTHON_VERSION}-debug/Python-${PYENV_PYTHON_VERSION}/python-gdb.py \
#       /opt/pyenv/versions/${PYENV_PYTHON_VERSION}-debug/bin/python3-gdb.py \
#       && ln -s /opt/pyenv/sources/${PYENV_PYTHON_VERSION}-debug/Python-${PYENV_PYTHON_VERSION}/python-gdb.py \
#       /opt/pyenv/versions/${PYENV_PYTHON_VERSION}-debug/bin/python-gdb.py
#   RUN apt-get -qq --yes --no-install-recommends --no-install-suggests install gdb
#   RUN echo add-auto-load-safe-path \
#       /opt/pyenv/sources/${PYENV_PYTHON_VERSION}-debug/Python-${PYENV_PYTHON_VERSION}/ \
#       >> etc/gdb/gdbinit
#  ##
#   Update pip and setuptools to the latest versions
#  ##
RUN pip install pip==23.1 setuptools==67.6.1 --upgrade
#  ##
#   Node
#  ##
#   RUN ln -s /usr/bin/nodejs /usr/bin/node
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash
RUN apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 -y
#  ##
#  # pa11y
#  ##
RUN wget https://bitbucket.org/ariya/phantomjs/downloads/phantomjs-2.1.1-linux-x86_64.tar.bz2 \
 && tar xvjf phantomjs-2.1.1-linux-x86_64.tar.bz2 -C /usr/local/share/ \
 && ln -s /usr/local/share/phantomjs-2.1.1-linux-x86_64/bin/phantomjs /usr/local/bin/
RUN npm install pa11y@4.13.2 --global --ignore-scripts
#  ##
#  # third_parties
#  ##
RUN npm install puppeteer@19.9.0
#  ##
#   Create unprivileged User
#  ##
ENV SCANNER_HOME="/home/scanner"
RUN mkdir $SCANNER_HOME \
 && groupadd -r scanner \
 && useradd -r -c "Scanner user" -g scanner scanner \
 && chown -R scanner:scanner ${SCANNER_HOME}
#  ##
#   Prepare to Run
#  ##
WORKDIR $SCANNER_HOME
#   Volume mount for use with the 'data' option.
VOLUME /data
COPY . $SCANNER_HOME
#  ##
#   domain-scan
#  ##
RUN pip install --upgrade -r requirements.txt -r requirements-gatherers.txt -r requirements-scanners.txt
#   Clean up aptitude stuff we no longer need
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/*
ENTRYPOINT ["./scan_wrap.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
