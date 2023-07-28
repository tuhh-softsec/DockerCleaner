FROM ubuntu:14.04
ENV DEBIAN_FRONTEND="noninteractive"
#   Install Cozy tools and dependencies.
RUN echo "deb http://ppa.launchpad.net/nginx/stable/ubuntu trusty main" >> /etc/apt/sources.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys C300EE8C \
 && apt-get update --quiet \
 && apt-get install --no-install-recommends build-essential=11.6ubuntu6 couchdb=1.5.0-0ubuntu1 curl=7.35.0-1ubuntu2.20 git=1:1.9.1-1ubuntu0.10 imagemagick=8:6.7.7.10-6ubuntu3.13 language-pack-en=1:14.04+20160720 libffi6=3.1~rc1+r3.0.13-12ubuntu0.2 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libssl-dev=1.0.1f-1ubuntu2.27 libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 libxslt1-dev=1.1.28-2ubuntu0.2 libjpeg-dev=8c-2ubuntu8 lsof=4.86+dfsg-1ubuntu2 nginx=1.4.6-1ubuntu3.9 postfix=2.11.0-1ubuntu1.2 pwgen=2.06-1ubuntu4 python-dev=2.7.5-5ubuntu3 python-pip=1.5.4-1ubuntu4 python-setuptools=3.3-1ubuntu2 python-software-properties=0.92.37.8 software-properties-common=0.92.37.8 sqlite3=3.8.2-1ubuntu2.2 wget=1.15-1ubuntu1.14.04.5 --quiet --yes
RUN update-locale LANG=en_US.UTF-8
RUN pip install supervisor==4.2.5 virtualenv==20.21.0
#   Install NodeJS 4.2.X LTS
RUN curl -sL https://deb.nodesource.com/setup_4.x | bash -
RUN apt-get install --no-install-recommends nodejs=0.10.25~dfsg2-2ubuntu1.2 -y
#   Install CoffeeScript, Cozy Monitor and Cozy Controller via NPM.
RUN npm install coffee-script@1.12.7 cozy-controller@2.5.10 cozy-monitor@1.4.5 -g
#   Create Cozy users, without home directories.
RUN useradd -M cozy \
 && useradd -M cozy-data-system \
 && useradd -M cozy-home
#   Configure CouchDB.
RUN mkdir /etc/cozy \
 && chown -hR cozy /etc/cozy
RUN pwgen -1 > /etc/cozy/couchdb.login \
 && pwgen -1 >> /etc/cozy/couchdb.login \
 && chown cozy-data-system /etc/cozy/couchdb.login \
 && chmod 640 /etc/cozy/couchdb.login
RUN mkdir /var/run/couchdb \
 && chown -hR couchdb /var/run/couchdb \
 && su - couchdb -c 'couchdb -b' \
 && sleep 5 \
 && while ! curl -s 127.0.0.1:5984 ; do sleep 5 ; done \
 && curl -s -X PUT 127.0.0.1:5984/_config/admins/$( head -n1 /etc/cozy/couchdb.login ;) -d "\"$( tail -n1 /etc/cozy/couchdb.login ;)\""
#   Configure Supervisor.
COPY supervisor/supervisord.conf /etc/supervisord.conf
RUN mkdir -p /var/log/supervisor \
 && chmod 777 /var/log/supervisor \
 && /usr/local/bin/supervisord -c /etc/supervisord.conf
#   Start up background services and install the Cozy platform apps.
ENV NODE_ENV="production"
RUN su - couchdb -c 'couchdb -b' \
 && sleep 5 \
 && while ! curl -s 127.0.0.1:5984 ; do sleep 5 ; done \
 && cozy-controller &; sleep 5 \
 && while ! curl -s 127.0.0.1:9002 ; do sleep 5 ; done \
 && cozy-monitor install data-system \
 && cozy-monitor install home \
 && cozy-monitor install proxy \
 && curl -X POST http://localhost:9103/api/instance -H "Content-Type: application/json" -d '{"background":"background-07"}' \
 && for app in calendar contacts photos emails files sync; do cozy-monitor install $app ; done
#   Configure Nginx and check its configuration by restarting the service.
COPY nginx/nginx.conf /etc/nginx/nginx.conf
COPY nginx/cozy /etc/nginx/sites-available/cozy
COPY nginx/cozy-ssl /etc/nginx/sites-available/cozy-ssl
RUN chmod 0644 /etc/nginx/sites-available/cozy /etc/nginx/sites-available/cozy-ssl \
 && rm /etc/nginx/sites-enabled/default \
 && ln -s /etc/nginx/sites-available/cozy /etc/nginx/sites-enabled/cozy
RUN nginx -t
#   Configure Postfix with default parameters.
ENV DISABLE_SSL="false"
ENV POSTFIX_DOMAIN="mydomain.net"
RUN echo "postfix postfix/mailname string $POSTFIX_DOMAIN" | debconf-set-selections \
 && echo "postfix postfix/main_mailer_type select Internet Site" | debconf-set-selections \
 && echo "postfix postfix/destinations string $POSTFIX_DOMAIN, localhost.localdomain, localhost " | debconf-set-selections \
 && cp /etc/services /var/spool/postfix/etc/ \
 && cp /etc/resolv.conf /var/spool/postfix/etc \
 && postfix check
#   Import Supervisor configuration files.
COPY supervisor/cozy-controller.conf /etc/supervisor/conf.d/cozy-controller.conf
COPY supervisor/cozy-init.conf /etc/supervisor/conf.d/cozy-init.conf
COPY supervisor/couchdb.conf /etc/supervisor/conf.d/couchdb.conf
COPY supervisor/nginx.conf /etc/supervisor/conf.d/nginx.conf
COPY supervisor/postfix.conf /etc/supervisor/conf.d/postfix.conf
COPY cozy-init /etc/init.d/cozy-init
RUN chmod 0644 /etc/supervisor/conf.d/*
#   Clean APT cache for a lighter image.
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
EXPOSE 80/tcp 443/tcp
VOLUME ["/var/lib/couchdb", "/etc/cozy", "/usr/local/cozy", "/usr/local/var/cozy/"]
CMD ["/usr/local/bin/supervisord", "-n", "-c", "/etc/supervisord.conf"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
