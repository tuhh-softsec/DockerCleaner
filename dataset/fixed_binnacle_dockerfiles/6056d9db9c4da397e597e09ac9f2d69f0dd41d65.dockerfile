FROM debian:buster-slim
MAINTAINER Michal Čihař <michal@cihar.com>
ENV VERSION="3.7"
LABEL version="$VERSION"
#   Add user early to get a consistent userid
RUN useradd --shell /bin/sh --user-group weblate \
 && mkdir -p /home/weblate/.ssh \
 && touch /home/weblate/.ssh/authorized_keys \
 && chown -R weblate:weblate /home/weblate \
 && chmod 700 /home/weblate/.ssh \
 && install -d -o weblate -g weblate -m 755 /usr/local/lib/python3.7/dist-packages/data-test \
 && install -d -o weblate -g weblate -m 755 /app/data
#   Configure utf-8 locales to make sure Python
#   correctly handles unicode filenames
ENV LANG="C.UTF-8" \
    LC_ALL="C.UTF-8"
COPY requirements.txt patches /usr/src/weblate/
#   Install dependencies
RUN set -x \
 && export DEBIAN_FRONTEND=noninteractive \
 && apt-get update \
 && apt-get -y upgrade \
 && apt-get install --no-install-recommends sudo=1.8.27-1+deb10u5 uwsgi=2.0.18-1 uwsgi-plugin-python3=2.0.18-1 netcat-openbsd=1.195-2 nginx=1.14.2-2+deb10u5 supervisor=3.3.5-1 openssh-client=1:7.9p1-10+deb10u2 curl=7.64.0-4+deb10u5 gir1.2-pango-1.0=1.42.4-8~deb10u1 python3-gi=3.30.4-1 python3-gi-cairo=3.30.4-1 python3-cairo=1.16.2-1+b1 python3-pip=18.1-5 python3-lxml=4.3.2-1+deb10u4 python3-yaml=3.13-2 python3-pillow python3-setuptools=40.8.0-1 python3-wheel=0.32.3-2 python3-gdbm=3.7.3-1 python3-psycopg2=2.7.7-1 python3-rcssmin=1.0.6-1+b3 python3-rjsmin=1.0.12+dfsg1-4+b2 gettext=0.19.8.1-9 postgresql-client=11+200+deb10u5 mercurial=4.8.2-1+deb10u1 git=1:2.20.1-2+deb10u8 git-svn=1:2.20.1-2+deb10u8 subversion=1.10.4-1+deb10u3 pkg-config=0.29-6 python3-dev=3.7.3-1 libxml2-dev=2.9.4+dfsg1-7+deb10u5 libxmlsec1-dev=1.2.27-2 libleptonica-dev=1.76.0-1+deb10u2 libtesseract-dev=4.0.0-2 libsasl2-dev=2.1.27+dfsg-1+deb10u2 libldap2-dev=2.4.47+dfsg-3+deb10u7 libssl-dev=1.1.1n-0+deb10u4 cython=0.29.2-2 gcc=4:8.3.0-1 g++=4:8.3.0-1 tesseract-ocr=4.0.0-2 patch=2.7.6-3+deb10u1 -y \
 && pip3 install Weblate==$VERSION -r /usr/src/weblate/requirements.txt \
 && python3 -c 'from phply.phpparse import make_parser; make_parser()' \
 && ln -s /usr/local/share/weblate/examples/ /app/ \
 && rm -rf /root/.cache /tmp/* \
 && apt-get -y purge python3-dev pkg-config libleptonica-dev libtesseract-dev libxml2-dev libxmlsec1-dev cython gcc g++ libsasl2-dev libldap2-dev libssl-dev \
 && apt-get -y autoremove \
 && apt-get clean
#   Hub
RUN curl -L https://github.com/github/hub/releases/download/v2.2.9/hub-linux-amd64-2.2.9.tgz | tar xzv --wildcards hub-linux*/bin/hub \
 && cp hub-linux-amd64-2.2.9/bin/hub /usr/bin \
 && rm -rf hub-linux-amd64-2.2.9
#   Configuration for Weblate, nginx, uwsgi and supervisor
COPY etc /etc/
RUN chmod a+r /etc/weblate/settings.py \
 && ln -s /etc/weblate/settings.py /usr/local/lib/python3.7/dist-packages/weblate/settings.py
#   Apply hotfixes
RUN find /usr/src/weblate -name '*.patch' -print0 | sort -z | xargs -n1 -0 -r patch -p1 -d /usr/local/lib/python3.7/dist-packages/ -i
#   Entrypoint
COPY start /app/bin/
RUN chmod a+rx /app/bin/start
EXPOSE 80/tcp
ENTRYPOINT ["/app/bin/start"]
CMD ["runserver"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
