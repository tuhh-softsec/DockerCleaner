#   vim:set ft=dockerfile:  
FROM alpine:latest
MAINTAINER DI GREGORIO Nicolas "nicolas.digregorio@gmail.com"  
#  ## Environment variables  
ENV LANG="en_US.UTF-8" \
    LANGUAGE="en_US.UTF-8" \
    TERM="xterm" \
    PYCURL_SSL_LIBRARY="openssl"
#  ## Install Application  
RUN apk --no-cache upgrade \
 && apk add make=4.3-r1 gcc=12.2.1_git20220924-r4 g++=12.2.1_git20220924-r4 python-dev py2-pip libressl-dev=3.6.2-r0 curl-dev=7.88.1-r1 musl-dev=1.2.3-r4 libffi-dev=3.4.4-r0 jpeg-dev=9e-r0 linux-headers=5.19.5-r0 dbus-dev=1.14.4-r0 dbus-glib-dev=0.112-r1 libev-dev=4.33-r0 autoconf=2.71-r1 nodejs=18.14.2-r0 git=2.38.4-r1 --no-cache --virtual=build-deps \
 && pip install setuptools==67.6.1 pip==23.1 --no-cache-dir --upgrade \
 && pip install argparse==1.4.0 beaker==1.12.1 bottle==0.12.25 daemonize==2.5.0 future==0.18.3 psutil==5.9.4 pycurl==7.45.2 requests==2.28.2 tld==0.13 validators==0.20.0 beautifulsoup4==4.12.2 bitmath==1.3.3.1 bjoern==3.2.2 colorama==0.4.6 colorlog==6.7.0 dbus-python==1.3.2 goslate==1.5.4 IPy==1.01 Js2Py==0.74 Pillow==9.5.0 pycrypto==2.6.1 pyOpenSSL==23.1.1 rarfile==4.0 Send2Trash==1.8.0 setproctitle==1.3.2 watchdog==3.0.0 webencodings==0.5.1 jinja2==3.1.2 html5lib==1.1 alabaster==0.7.13 Pygments==2.15.0 docutils==0.19 bleach==6.0.0 snowballstemmer==2.2.0 imagesize==1.4.1 Babel==2.12.1 readme_renderer==37.3 configparser==5.3.0 autoupgrade-ng==0.3.0 Sphinx==6.1.3 --no-cache-dir --upgrade \
 && git clone --depth 1 https://github.com/pyload/pyload.git /opt/pyload \
 && cd /opt/pyload/pyload/webui \
 && npm install \
 && cd /opt/pyload \
 && python setup.py install \
 && apk del --no-cache --purge build-deps \
 && apk add python nodejs=18.14.2-r0 ssmtp=2.64-r18 mailx=8.1.1-r3 dbus=1.14.4-r0 dbus-glib=0.112-r1 libev=4.33-r0 libffi=3.4.4-r0 libcurl=7.88.1-r1 jpeg=9e-r0 unrar su-exec=0.2-r2 --no-cache --virtual=run-deps \
 && rm -rf /tmp/* /opt/pyload/.git /var/cache/apk/* /var/tmp/*
#  ## Volume  
VOLUME ["/downloads","/config"]  
#  ## Expose ports  
EXPOSE 8000/tcp 7227/tcp
#  ## Running User: not used, managed by docker-entrypoint.sh  
#  USER pyload  
#  ## Start pyload  
COPY ./docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["pyload"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
