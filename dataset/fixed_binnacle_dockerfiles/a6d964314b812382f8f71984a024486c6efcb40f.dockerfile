FROM ubuntu:16.04
MAINTAINER Shane Frasier <jeremy.frasier@trio.dhs.gov>
ENV DEBIAN_FRONTEND="noninteractive" \
    LANG="C.UTF-8"
#  ##
#   Dependencies
#  ##
RUN apt-get update -qq \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 libc6-dev=2.23-0ubuntu11.3 libfontconfig1=2.11.94-0ubuntu1.1 libreadline-dev=6.3-8ubuntu2 libssl-dev=1.0.2g-1ubuntu4.20 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxslt1-dev=1.1.28-2.1ubuntu0.3 libyaml-dev=0.1.6-3 make=4.1-6 unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 autoconf=2.69-9 automake=1:1.15-4ubuntu1 bison=2:3.0.4.dfsg-1 gawk=1:4.1.3+dfsg-0.1 libffi-dev=3.2.1-4 libgdbm-dev=1.8.3-13.1 libncurses5-dev=6.0+20160213-1ubuntu1 libsqlite3-dev=3.11.0-1ubuntu1.5 libtool=2.4.6-0.1 pkg-config=0.29.1-0ubuntu1 sqlite3=3.11.0-1ubuntu1.5 libgeos-dev=3.5.0-1ubuntu2 libbz2-dev=1.0.6-8ubuntu0.2 llvm=1:3.8-33ubuntu3.1 libncursesw5-dev=6.0+20160213-1ubuntu1 nodejs=4.2.6~dfsg-1ubuntu4.2 npm=3.5.2-0ubuntu4 redis-tools=2:3.0.6-1ubuntu0.4 -qq --yes --no-install-suggests
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
#   ENV PATH=$PYENV_ROOT/bin:$PYENV_ROOT/versions/${PYENV_PYTHON_VERSION}-debug/bin:$PATH
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
#  #
#   Make sure pip and setuptools are the latest versions
#  #
RUN pip install pip==23.1 setuptools==67.6.1 --upgrade
#  #
#   We're using Lambda, but we need to install pshtt locally because the
#   pshtt.py and sslyze.py files in the scanners directory of
#   18F/domain-scan import pshtt and sslyze, respectively, at the top of
#   the file.  (trustymail imports only in the scan function, so it
#   isn't required here.)
#  #
RUN pip install pshtt==0.6.1 --upgrade
#  ##
#   Install domain-scan
#  ##
RUN git clone https://github.com/18F/domain-scan /home/scanner/domain-scan/ \
 && pip install --upgrade -r /home/scanner/domain-scan/requirements.txt
#  ##
#   Create unprivileged user
#  ##
ENV SCANNER_HOME="/home/scanner"
RUN groupadd -r scanner \
 && useradd -r -c "Scanner user" -g scanner scanner
#   It would be nice to get rid of some build dependencies at this point
#   Clean up aptitude cruft
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Put this just before we change users because the copy (and every
#   step after it) will always be rerun by docker, but we need to be
#   root for the chown command.
COPY . $SCANNER_HOME
RUN chown -R scanner:scanner ${SCANNER_HOME}
#  ##
#   Prepare to Run
#  ##
#   Right now we need to be root at runtime in order to create files in
#   /home/shared
#   USER scanner:scanner
WORKDIR $SCANNER_HOME
ENTRYPOINT ["./scan.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
