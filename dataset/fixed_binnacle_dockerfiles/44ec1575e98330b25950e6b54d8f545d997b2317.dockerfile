FROM alpine:3.4
#   ensure local python is preferred over distribution python
ENV PATH="/usr/local/bin:$PATH"
#   http://bugs.python.org/issue19846
#   > At the moment, setting "LANG=C" on a Linux system *fundamentally breaks Python 3*, and that's not OK.
ENV LANG="C.UTF-8"
#   install ca-certificates so that HTTPS works consistently
#   the other runtime dependencies for Python are installed later
RUN apk add ca-certificates=20161130-r0 --no-cache
ENV GPG_KEY="C01E1CAD5EA2C4F0B8E3571504C367C218ADD4FF"
ENV PYTHON_VERSION="2.7.12"
#   if this is called "PIP_VERSION", pip explodes with "ValueError: invalid truth value '<VERSION>'"
ENV PYTHON_PIP_VERSION="9.0.1"
RUN set -ex \
 && buildDeps=' tcl-dev tk-dev ' \
 && apt-get update \
 && apt-get install --no-install-recommends $buildDeps -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -O python.tar.xz "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz" \
 && wget -O python.tar.xz.asc "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$GPG_KEY" \
 && gpg --batch --verify python.tar.xz.asc python.tar.xz \
 && rm -r "$GNUPGHOME" python.tar.xz.asc \
 && mkdir -p /usr/src/python \
 && tar -xJC /usr/src/python --strip-components=1 -f python.tar.xz \
 && rm python.tar.xz \
 && cd /usr/src/python \
 && ./configure --enable-shared --enable-unicode=ucs4 \
 && make -j$( nproc ;) \
 && make install \
 && ldconfig \
 && wget -O /tmp/get-pip.py 'https://bootstrap.pypa.io/get-pip.py' \
 && python2 /tmp/get-pip.py "pip==$PYTHON_PIP_VERSION" \
 && rm /tmp/get-pip.py \
 && pip install "pip==$PYTHON_PIP_VERSION" --no-cache-dir --upgrade --force-reinstall \
 && [ "$( pip list | tac | tac | awk -F '[ ()]+' '$1 == "pip" { print $2; exit }' ;)" = "$PYTHON_PIP_VERSION" ] \
 && find /usr/local -depth
#   install gosu
ENV GOSU_VERSION="1.9"
RUN set -x \
 && apk add dpkg=1.18.7-r0 gnupg=2.1.12-r1 openssl=1.0.2n-r0 --no-cache --virtual .gosu-deps \
 && dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" \
 && wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch" \
 && wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
 && gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
 && rm -r "$GNUPGHOME" /usr/local/bin/gosu.asc \
 && chmod +x /usr/local/bin/gosu \
 && gosu nobody true \
 && apk del .gosu-deps
RUN apk add nginx=1.10.3-r0 uwsgi=2.0.17-r0 uwsgi-python=2.0.17-r0 python-dev=2.7.14-r0 build-base=0.4-r1 dumb-init --no-cache
RUN pip install flask==2.2.3 connexion==2.14.2 uwsgi==2.0.21
RUN useradd uwsgi -s /bin/false
RUN mkdir /var/log/uwsgi
RUN chown -R uwsgi:uwsgi /var/log/uwsgi
ONBUILD ADD . /
ONBUILD RUN if [ -f /requirements.txt ] ; then pip install -r /requirements.txt ; fi
ENV UWSGI_NUM_PROCESSES="1"
ENV UWSGI_NUM_THREADS="15"
ENV UWSGI_UID="uwsgi"
ENV UWSGI_GID="uwsgi"
ENV UWSGI_LOG_FILE="/var/log/uwsgi/uwsgi.log"
EXPOSE 8080/tcp
COPY uwsgi-start.sh /
#  CMD        []
#  ENTRYPOINT ["/uwsgi-start.sh"]
ENTRYPOINT ["/usr/bin/dumb-init", "--"]
CMD ["/uwsgi-start.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
