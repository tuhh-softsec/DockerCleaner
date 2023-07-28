#  # --------------------------------------------------------------------------------------------
#  # ############################################################################################
#  # --------------------------------------------------------------------------------------------
FROM ubuntu:trusty
MAINTAINER Hasan Karahan <hasan.karahan@blackhan.com>
#  # --------------------------------------------------------------------------------------------
#  # Part (a): `notex:nil` ######################################################################
#  # --------------------------------------------------------------------------------------------
#   ubuntu: updates & upgrades
RUN :
RUN apt-get -y upgrade
#   locale: `en_US.UTF-8`
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
#   basic tools
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 -y )
RUN (apt-get update ;apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 -y )
RUN (apt-get update ;apt-get install --no-install-recommends zip=3.0-8 -y )
RUN (apt-get update ;apt-get install --no-install-recommends unzip=6.0-9ubuntu1.5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends zlibc=0.9k-4.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.15-1ubuntu1.14.04.5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.35.0-1ubuntu2.20 -y )
RUN (apt-get update ;apt-get install --no-install-recommends nano=2.2.6-1ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends sudo=1.8.9p5-1ubuntu1.4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends tree=1.6.0-1 -y )
RUN git config --global user.name "NoTex.ch"
RUN git config --global user.email "contact@blackhan.com"
#   java: 7u55
RUN wget -O jdk-7u55-linux-x64.tar.gz https://db.tt/d8AltmDK
RUN tar -xvf *-linux-x64.tar.gz \
 && mkdir -p /usr/lib/jvm
RUN mv ./jdk1.7.0_55 /usr/lib/jvm/jdk1.7.0 \
 && mv *.tar.gz /root/
RUN update-alternatives --install "/usr/bin/java" "java" "/usr/lib/jvm/jdk1.7.0/bin/java" 1
RUN update-alternatives --install "/usr/bin/javac" "javac" "/usr/lib/jvm/jdk1.7.0/bin/javac" 1
RUN update-alternatives --install "/usr/bin/javaws" "javaws" "/usr/lib/jvm/jdk1.7.0/bin/javaws" 1
RUN chmod a+x /usr/bin/java /usr/bin/javac /usr/bin/javaws
RUN chown -R root:root /usr/lib/jvm/jdk1.7.0
#   ruby: 1.9.1
RUN (apt-get update ;apt-get install --no-install-recommends ruby=1:1.9.3.4 -y )
#   sencha command: 3.0.2.288
ENV SENCHA_CMD_x32="http://cdn.sencha.com/cmd/3.0.2.288/SenchaCmd-3.0.2.288-linux.run.zip"
ENV SENCHA_CMD_x64="http://cdn.sencha.com/cmd/3.0.2.288/SenchaCmd-3.0.2.288-linux-x64.run.zip"
RUN wget -O 3.0.2.288-linux-x64.run.zip $SENCHA_CMD_x64
RUN unzip *.run.zip \
 && rm *.run.zip \
 && chmod +x *.run
RUN mkdir -p /opt/Sencha/Cmd \
 && mv *.run /opt/Sencha/Cmd
RUN /opt/Sencha/Cmd/SenchaCmd-3.0.2.288-linux-x64.run --prefix /opt --mode unattended
ENV PATH="/opt/Sencha/Cmd/3.0.2.288:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
#   memcached: 1.4.14 & 1.0.8
RUN (apt-get update ;apt-get install --no-install-recommends memcached=1.4.14-0ubuntu9.3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libmemcached-dev=1.0.8-1ubuntu2 -y )
#   redis: 2.8.4
RUN (apt-get update ;apt-get install --no-install-recommends redis-server=2:2.8.4-2ubuntu0.2 -y )
#   postgresql: 9.3
RUN (apt-get update ;apt-get install --no-install-recommends postgresql=9.3+154ubuntu1.1 postgresql-server-dev-9.3=9.3.24-0ubuntu0.14.04 -y )
#   python: 2.7.6, pip: 1.5.6, virtualenv: 1.11, sphinx: 1.2.2
RUN (apt-get update ;apt-get install --no-install-recommends python2.7=2.7.6-8ubuntu0.5 python2.7-dev=2.7.6-8ubuntu0.5 python-sphinx=1.2.2+dfsg-1ubuntu1.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python-pip=1.5.4-1ubuntu4 -y ) \
 && pip install virtualenv==20.21.0
RUN ln -s /usr/bin/sphinx-build /usr/bin/sphinx-build2
#   clean & remove
RUN apt-get -y clean
RUN apt-get -y autoclean
RUN apt-get -y autoremove
#  # --------------------------------------------------------------------------------------------
#  # Part (b): `notex:tex` ######################################################################
#  # --------------------------------------------------------------------------------------------
RUN (apt-get update ;apt-get install --no-install-recommends texlive-full=2013.20140215-1ubuntu0.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends ttf-* -y )
#  # --------------------------------------------------------------------------------------------
#  # Part (c): `notex:dev` ######################################################################
#  # --------------------------------------------------------------------------------------------
#   notex: fetch latest tag
RUN git clone https://github.com/hsk81/notex /srv/notex.git \
 && cd /srv/notex.git \
 && git checkout master \
 && git submodule update --init --recursive \
 && git checkout $( git tag | tail -n1 ;) \
 && git checkout -b tag-$( git tag | tail -n1 ;)
#   notex: setup postgres
RUN /etc/init.d/postgresql start ; cd /srv/notex.git \
 && sudo -u postgres -g postgres psql -f webed/config/pg.sql \
 && sudo -u postgres -g postgres psql -f webed/config/test.sql \
 && sudo -u postgres -g postgres psql -f webed/config/production.sql
#   notex: setup sencha & python env
RUN cd /srv/notex.git \
 && /bin/bash -c './setup.sh \
 && source bin/activate \
 && ./setup.py install' \
 && /bin/bash -c 'mkdir -p .python-eggs'
#   notex: setup dirs & owners
RUN mkdir -p /var/www/webed
RUN chown www-data:www-data /var/www/webed -R
RUN chown www-data:www-data /srv/notex.git -R
#   notex: execute `reset`
RUN /etc/init.d/memcached restart \
 && /etc/init.d/redis-server restart \
 && /etc/init.d/postgresql restart \
 && cd /srv/notex.git \
 && /bin/bash -c 'source bin/activate \
 && /usr/bin/sudo -u www-data -g www-data PYTHON_EGG_CACHE=.python-eggs WEBED_SETTINGS=/srv/notex.git/webed/config/production.py ./webed.py reset'
#  # --------------------------------------------------------------------------------------------
#  # Part (d): `notex:pro` ######################################################################
#  # --------------------------------------------------------------------------------------------
#   lighttpd: 1.4.33, highlight: 3.9
RUN (apt-get update ;apt-get install --no-install-recommends lighttpd=1.4.33-1+nmu2ubuntu2.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends highlight=3.9-1build1 -y )
#   nginx: 1.4.6
RUN (apt-get update ;apt-get install --no-install-recommends nginx=1.4.6-1ubuntu3.9 -y )
RUN rm -rf /etc/nginx/sites-enabled/*
RUN rm -rf /etc/nginx/conf.d/*
#   notex: execute `assets build`
RUN cd /srv/notex.git \
 && /bin/bash -c 'source bin/activate \
 && /usr/bin/sudo -u www-data -g www-data PYTHON_EGG_CACHE=.python-eggs WEBED_SETTINGS=/srv/notex.git/webed/config/production.py ./webed.py assets build'
#   notex: execute `assets-gzip`
RUN cd /srv/notex.git \
 && /bin/bash -c 'source bin/activate \
 && /usr/bin/sudo -u www-data -g www-data PYTHON_EGG_CACHE=.python-eggs WEBED_SETTINGS=/srv/notex.git/webed/config/production.py ./webed.py assets-gzip'
#  # --------------------------------------------------------------------------------------------
#  # Part (e): `notex:run` ######################################################################
#  # --------------------------------------------------------------------------------------------
#   notex: `webed.run`
RUN cd /srv/notex.git \
 && echo '#!/bin/bash\nif [[ $@ =~ NGINX=(1|true) ]] ; then /etc/init.d/nginx restart 2> /dev/null ; fi\nif [[ $@ =~ LIGHTTPD=(1|true) ]] ; then /etc/init.d/lighttpd restart 2> /dev/null ; fi\nif [[ $@ =~ REDIS=(1|true) ]] ; then /etc/init.d/redis-server restart 2> /dev/null ; fi\nif [[ $@ =~ MEMCACHED=(1|true) ]] ; then /etc/init.d/memcached restart 2> /dev/null ; fi\nif [[ $@ =~ POSTGRESQL=(1|true) ]] ; then /etc/init.d/postgresql restart 2> /dev/null ; fi\nif [[ $@ =~ GITDAEMON=(1|true) ]] ; then /bin/sh -e /etc/rc.local 2> /dev/null ; fi\n\ncd /srv/notex.git \
 && CMD=$@ \
 && /usr/bin/sudo -u www-data -g www-data /bin/bash -c "source bin/activate \
 && PYTHON_EGG_CACHE=.python-eggs WEBED_SETTINGS=/srv/notex.git/webed/config/production.py $CMD"\n' > webed.run \
 && chmod +x webed.run
#   notex: execute `webed.run`
ENTRYPOINT ["/srv/notex.git/webed.run"]
#  # --------------------------------------------------------------------------------------------
#  # Part (f): `notex:cfg` ######################################################################
#  # --------------------------------------------------------------------------------------------
COPY nginx.conf /etc/nginx/nginx.conf
COPY webed.conf /etc/nginx/conf.d/webed.conf
COPY robots.txt /etc/nginx/conf.d/robots.txt
COPY lighttpd.conf /etc/lighttpd/lighttpd.conf
COPY gitweb.conf /etc/gitweb.conf
COPY rc.local /etc/rc.local
COPY webed/config/default.py /srv/notex.git/webed/config/default.py
COPY webed/config/production.py /srv/notex.git/webed/config/production.py
COPY webed/config/sphinx.py /srv/notex.git/webed/config/sphinx.py
COPY webed/config/test.py /srv/notex.git/webed/config/test.py
#  # --------------------------------------------------------------------------------------------
#  # ############################################################################################
#  # --------------------------------------------------------------------------------------------
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
