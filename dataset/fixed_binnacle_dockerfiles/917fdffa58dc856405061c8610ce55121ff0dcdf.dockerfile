FROM ubuntu:14.04
MAINTAINER Donghyun Seo <egaoneko@naver.com>
#   Set the locale
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 git=1:1.9.1-1ubuntu0.10 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python=2.7.5-5ubuntu3 python-dev=2.7.5-5ubuntu3 python3=3.4.0-0ubuntu2 python3-dev=3.4.0-0ubuntu2 python3-pip=1.5.4-1ubuntu4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends nginx=1.4.6-1ubuntu3.9 supervisor=3.0b2-1ubuntu0.1 -y )
RUN pip3 install glances
RUN echo "\ndaemon off;" >> /etc/nginx/nginx.conf
RUN chown -R www-data:www-data /var/lib/nginx
#   UWSGI
ENV UWSGIVERSION="2.0.11.2"
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 libjansson-dev=2.5-2ubuntu0.2 libpcre3-dev=1:8.31-2ubuntu2.3 libssl-dev=1.0.1f-1ubuntu2.27 libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 wget=1.15-1ubuntu1.14.04.5 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 -y )
RUN cd /usr/src \
 && wget --quiet -O - http://projects.unbit.it/downloads/uwsgi-${UWSGIVERSION}.tar.gz | tar zxvf -
RUN cd /usr/src/uwsgi-${UWSGIVERSION} \
 && make
RUN cp /usr/src/uwsgi-${UWSGIVERSION}/uwsgi /usr/local/bin/uwsgi
RUN PYTHON=/usr/local/python3.4/bin/python3.4
RUN cd /usr/src/uwsgi-${UWSGIVERSION} \
 && ./uwsgi --build-plugin "plugins/python python34"
RUN mkdir -p /usr/local/lib/uwsgi/plugins
RUN cp /usr/src/uwsgi-${UWSGIVERSION}/*.so /usr/local/lib/uwsgi/plugins
RUN pip3 install uwsgi
#   Java
ENV VERSION="7"
ENV UPDATE="80"
ENV BUILD="15"
ENV JAVA_HOME="/usr/lib/jvm/java-${VERSION}-oracle"
ENV JRE_HOME="${JAVA_HOME}/jre"
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends ca-certificates=20170717~14.04.2 curl=7.35.0-1ubuntu2.20 -y ) \
 && curl --silent --location --retry 3 --cacert /etc/ssl/certs/GeoTrust_Global_CA.pem --header "Cookie: oraclelicense=accept-securebackup-cookie;" http://download.oracle.com/otn-pub/java/jdk/"${VERSION}"u"${UPDATE}"-b"${BUILD}"/server-jre-"${VERSION}"u"${UPDATE}"-linux-x64.tar.gz | tar xz -C /tmp \
 && mkdir -p /usr/lib/jvm \
 && mv /tmp/jdk1.${VERSION}.0_${UPDATE} "${JAVA_HOME}" \
 && apt-get autoclean \
 && apt-get --purge -y autoremove \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN update-alternatives --install "/usr/bin/java" "java" "${JRE_HOME}/bin/java" 1 \
 && update-alternatives --install "/usr/bin/javac" "javac" "${JAVA_HOME}/bin/javac" 1 \
 && update-alternatives --set java "${JRE_HOME}/bin/java" \
 && update-alternatives --set javac "${JAVA_HOME}/bin/javac"
#   Node
RUN apt-get update -qq \
 && (apt-get update ;apt-get install --no-install-recommends ca-certificates=20170717~14.04.2 curl=7.35.0-1ubuntu2.20 nodejs=0.10.25~dfsg2-2ubuntu1.2 git=1:1.9.1-1ubuntu0.10 make=3.81-8.2ubuntu3 g++=4:4.8.2-1ubuntu6 -qq -y ) \
 && ln -s /usr/bin/nodejs /usr/local/bin/node \
 && curl -skLN https://npmjs.org/install.sh | /bin/bash \
 && rm -rf /var/lib/apt/lists/*
RUN npm install bower@1.8.14 -g
#   Redis
RUN apt-get update -qq \
 && (apt-get update ;apt-get install --no-install-recommends python-software-properties=0.92.37.8 sudo=1.8.9p5-1ubuntu1.4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends redis-server=2:2.8.4-2ubuntu0.2 -y )
RUN sysctl vm.overcommit_memory=1 > /dev/null
RUN chown -R redis:redis /var/lib/redis
#   psycopg2
RUN (apt-get update ;apt-get install --no-install-recommends python-psycopg2=2.4.5-1build5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libpq-dev=9.3.24-0ubuntu0.14.04 -y )
#   lxml
RUN (apt-get update ;apt-get install --no-install-recommends python3-lxml=3.3.3-1ubuntu0.2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 libxslt-dev -y )
#   Pillow
RUN (apt-get update ;apt-get install --no-install-recommends libjpeg8=8c-2ubuntu8 libjpeg62-dev=6b1-4ubuntu1 libfreetype6=2.5.2-1ubuntu2.8 libfreetype6-dev=2.5.2-1ubuntu2.8 -y )
#   Memcached
RUN (apt-get update ;apt-get install --no-install-recommends libmemcached-dev=1.0.8-1ubuntu2 -y )
#   Mecab
#   RUN apt-get install curl
#   RUN bash <(curl -s https://raw.githubusercontent.com/konlpy/konlpy/master/scripts/mecab.sh)
#   Project
ENV PROJECT_DIR="/home/ubuntu/workspace/ward"
COPY . ${PROJECT_DIR}
RUN chown -R www-data:www-data ${PROJECT_DIR}
RUN cd ${PROJECT_DIR} \
 && bower --allow-root install
RUN cd ${PROJECT_DIR} \
 && pip3 install -r requirements.txt
RUN cd ${PROJECT_DIR}/www \
 && python3 manage.py migrate --noinput
RUN cd ${PROJECT_DIR}/www \
 && python3 manage.py collectstatic --noinput
RUN mkdir -p /var/uwsgi/sites-available
RUN ln -s ${PROJECT_DIR}/conf/nginx-app.conf /etc/nginx/sites-enabled/
RUN ln -s ${PROJECT_DIR}/conf/uwsgi.ini /var/uwsgi/sites-available/
RUN cp ${PROJECT_DIR}/conf/celeryd.conf /etc/default/celeryd
RUN cp ${PROJECT_DIR}/conf/celerybeat.conf /etc/default/celerybeat
RUN cp ${PROJECT_DIR}/conf/redis.conf /etc/redis/redis.conf
RUN ln -s ${PROJECT_DIR}/conf/supervisor-app.conf /etc/supervisor/conf.d/
RUN cp ${PROJECT_DIR}/conf/celeryd /etc/init.d/
RUN chmod +x /etc/init.d/celeryd
RUN update-rc.d celeryd defaults
RUN update-rc.d celeryd enable
RUN chown root:root /etc/init.d/celeryd
RUN chmod 755 /etc/init.d/celeryd
RUN cp ${PROJECT_DIR}/conf/celerybeat /etc/init.d/
RUN chmod +x /etc/init.d/celerybeat
RUN update-rc.d celerybeat defaults
RUN update-rc.d celerybeat enable
RUN chown root:root /etc/init.d/celerybeat
RUN chmod 755 /etc/init.d/celerybeat
VOLUME ["/data", "/var/log",  "/etc/nginx/site-enabled", "/var/log/nginx",  "/etc/uwsgi/apps-enabled", "/var/log/uwsgi",  "/var/log/celery", "/var/log/supervisor",  "/var/lib/redis", "/etc/redis"]
EXPOSE 80/tcp
EXPOSE 443/tcp
CMD ["supervisord", "-n"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
