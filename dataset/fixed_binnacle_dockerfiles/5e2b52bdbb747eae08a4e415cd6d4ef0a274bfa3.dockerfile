FROM ubuntu:14.04
MAINTAINER Yang 
RUN :
#  ## https://github.com/kstaken/dockerfile-examples/blob/master/mysql-server/Dockerfile
COPY ./mysql-setup.sh /tmp/mysql-setup.sh
RUN /bin/sh /tmp/mysql-setup.sh
CMD ["/usr/sbin/mysqld"]
#  # python related
RUN (apt-get update ;apt-get install --no-install-recommends python-software-properties=0.92.37.8 python=2.7.5-5ubuntu3 python-setuptools=3.3-1ubuntu2 git=1:1.9.1-1ubuntu0.10 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libmysqlclient-dev=5.5.62-0ubuntu0.14.04.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 libxslt1-dev=1.1.28-2ubuntu0.2 python-dev=2.7.5-5ubuntu3 -y )
RUN easy_install pip
RUN mkdir -p /www/mypocket/static/dowload-image
#  # nodejs
RUN add-apt-repository ppa:chris-lea/node.js
RUN echo "deb http://us.archive.ubuntu.com/ubuntu/ precise universe" >> /etc/apt/sources.list
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=0.10.25~dfsg2-2ubuntu1.2 -y )
RUN npm install less@4.1.3 requirejs@2.3.6 -g
#  # Redis
#  ## https://github.com/dockerfile/redis/blob/master/Dockerfile
#   Install Redis.
RUN cd /tmp \
 && wget http://download.redis.io/redis-stable.tar.gz \
 && tar xvzf redis-stable.tar.gz \
 && cd redis-stable \
 && make \
 && make install \
 && cp -f src/redis-sentinel /usr/local/bin \
 && mkdir -p /etc/redis \
 && cp -f *.conf /etc/redis \
 && rm -rf /tmp/redis-stable* \
 && sed -i 's/^\(bind .*\)$/# \1/' /etc/redis/redis.conf \
 && sed -i 's/^\(daemonize .*\)$/# \1/' /etc/redis/redis.conf \
 && sed -i 's/^\(dir .*\)$/# \1\ndir \/data/' /etc/redis/redis.conf \
 && sed -i 's/^\(logfile .*\)$/# \1/' /etc/redis/redis.conf
#   Define mountable directories.
VOLUME ["/data"]
#   Define working directory.
WORKDIR /data
#   Define default command.
CMD ["redis-server", "/etc/redis/redis.conf"]
#   Expose ports.
EXPOSE 6379/tcp
#  ## nginx
RUN add-apt-repository -y ppa:nginx/stable \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends nginx=1.4.6-1ubuntu3.9 -y ) \
 && rm -rf /var/lib/apt/lists/* \
 && echo "\ndaemon off;" >> /etc/nginx/nginx.conf \
 && chown -R www-data:www-data /var/lib/nginx
#   Define mountable directories.
VOLUME ["/etc/nginx/sites-enabled", "/etc/nginx/certs", "/etc/nginx/conf.d", "/var/log/nginx", "/var/www/html"]
#   Define working directory.
WORKDIR /etc/nginx
#   Define default command.
CMD ["nginx"]
#   Expose ports.
EXPOSE 80/tcp
EXPOSE 443/tcp
#  # project
RUN git clone https://github.com/zhy0216/OhMyPocket.git
WORKDIR /OhMyPocket
RUN pip install -r dev_requirements.txt
RUN fab compile_less \
 && fab compile_js
RUN python myworker.py > worklog&
RUN python manage.py syncdb
RUN python manage.py runserver
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
