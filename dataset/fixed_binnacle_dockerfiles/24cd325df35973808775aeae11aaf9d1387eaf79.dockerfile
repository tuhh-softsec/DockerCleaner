FROM ubuntu:14.04.5
MAINTAINER Cloud9 IDE, inc. <info@c9.io>
ENV DEBIAN_FRONTEND="noninteractive"
#   increment version to force flushing the cache
RUN echo "Version 1.13"
#   Set mirrors to automatic based off location
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.35.0-1ubuntu2.20 -y )
COPY files/auto-sources.sh /var/tmp/auto-sources.sh
#  RUN bash /var/tmp/auto-sources.sh && rm /var/tmp/auto-sources.sh
#   Install add-apt-repository script
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.92.37.8 apt-transport-https=1.0.1ubuntu2.24 -y )
#   Add a recent version of git
RUN add-apt-repository -y ppa:git-core/ppa
#   32-bit support
RUN dpkg --add-architecture i386 \
 && apt-get update --fix-missing \
 && (apt-get update ;apt-get install --no-install-recommends libc6:i386 libncurses5:i386 libstdc++6:i386 -y )
RUN (apt-get update ;apt-get install --no-install-recommends bash=4.3-7ubuntu1.7 console-setup=1.70ubuntu8 sudo=1.8.9p5-1ubuntu1.4 wget=1.15-1ubuntu1.14.04.5 curl=7.35.0-1ubuntu2.20 git=1:1.9.1-1ubuntu0.10 python=2.7.5-5ubuntu3 python-pip=1.5.4-1ubuntu4 python3=3.4.0-0ubuntu2 python3-pip=1.5.4-1ubuntu4 man-db=2.6.7.1-1ubuntu1 -y ) \
 && echo "install console-setup" \
 && echo "console-setup console-setup/codeset47 select # Latin1 and Latin5 - western Europe and Turkic languages" | debconf-set-selections \
 && echo "console-setup console-setup/fontface47 select Fixed" | debconf-set-selections \
 && echo "console-setup console-setup/fontsize-text47 select 16" | debconf-set-selections \
 && echo "console-setup console-setup/charmap47 select UTF-8" | debconf-set-selections \
 && echo "keyboard-configuration console-setup/detect detect-keyboard" | debconf-set-selections \
 && echo "keyboard-configuration console-setup/detected note" | debconf-set-selections \
 && echo "console-setup console-setup/codesetcode string Lat15" | debconf-set-selections \
 && echo "keyboard-configuration console-setup/ask_detect boolean false" | debconf-set-selections \
 && echo "console-setup console-setup/store_defaults_in_debconf_db boolean true" | debconf-set-selections \
 && echo "console-setup console-setup/fontsize-fb47 select 16" | debconf-set-selections \
 && echo "console-setup console-setup/fontsize string 16" | debconf-set-selections \
 && echo "Create user and enable root access" \
 && useradd --uid 1000 --shell /bin/bash -m --home-dir /home/ubuntu ubuntu \
 && sed -i 's/%sudo\s.*/%sudo ALL=NOPASSWD:ALL/' /etc/sudoers \
 && usermod -a -G sudo ubuntu
#   node.js and nvm
USER ubuntu
RUN curl https://raw.githubusercontent.com/creationix/nvm/v0.31.0/install.sh | sh -e \
 && echo '[ -s "/home/ubuntu/.nvm/nvm.sh" ] \
 && . "/home/ubuntu/.nvm/nvm.sh" # This loads nvm' >> /home/ubuntu/.profile
RUN bash -l -c " nvm install 6 \
 && nvm alias default 6 nvm use 6"
#   Disable progress bars by default: https://github.com/npm/npm/issues/11283
RUN bash -l -c "npm set progress=false"
USER root
#   Java
RUN (apt-get update ;apt-get install --no-install-recommends openjdk-7-jre=7u211-2.6.17-0ubuntu0.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends ant=1.9.3-2ubuntu0.1 maven2=2.2.1-19 -y )
#   editors
RUN (apt-get update ;apt-get install --no-install-recommends nano=2.2.6-1ubuntu1 vim=2:7.4.052-1ubuntu3.1 vim-addon-manager=0.5.3 vim-vimoutliner=0.3.4+pristine-9.1 vim-doc=2:7.4.052-1ubuntu3.1 ctags indent=2.2.11-4 -y )
#   servers
RUN (apt-get update ;apt-get install --no-install-recommends redis-server=2:2.8.4-2ubuntu0.2 redis-tools=2:2.8.4-2ubuntu0.2 nginx=1.4.6-1ubuntu3.9 mysql-server=5.5.62-0ubuntu0.14.04.1 rabbitmq-server=3.2.4-1ubuntu0.1 couchdb=1.5.0-0ubuntu1 -y )
#   Postgres
RUN (apt-get update ;apt-get install --no-install-recommends postgresql-9.3=9.3.24-0ubuntu0.14.04 -y ) \
 && /etc/init.d/postgresql start \
 && sudo -u postgres createuser -srd ubuntu \
 && sudo -u postgres psql -c "create database ubuntu owner=ubuntu"
#   misc tools
RUN (apt-get update ;apt-get install --no-install-recommends dnsutils=1:9.9.5.dfsg-3ubuntu0.19 bash-completion=1:2.1-4ubuntu0.2 xsltproc=1.1.28-2ubuntu0.2 build-essential=11.6ubuntu6 fakeroot=1.20-3ubuntu2 tmux=1.8-5 duplicity=0.6.23-1ubuntu4.1 lftp=4.4.13-1ubuntu0.1 htop=1.0.2-3 apt-file=2.5.2ubuntu1 parallel=20161222-1~ubuntu0.14.04.2 strace=4.8-1ubuntu5 ltrace=0.7.3-4ubuntu5.1 flex=2.5.35-10.1ubuntu2 jq=1.3-1.1ubuntu1.1 ack-grep=2.12-1 gdb=7.7.1-0ubuntu5~14.04.3 valgrind=1:3.10.1-1ubuntu3~14.5 locate=4.4.2-7 tree=1.6.0-1 time=1.7-24 zip=3.0-8 unp=2.0~pre7+nmu1 cmake=2.8.12.2-0ubuntu3 -y )
RUN apt-file update
#   Version control
RUN (apt-get update ;apt-get install --no-install-recommends git-all=1:1.9.1-1ubuntu0.10 tig=1.2.1-1 subversion=1.8.8-1ubuntu3.3 subversion-tools=1.8.8-1ubuntu3.3 cvs=2:1.12.13+real-12ubuntu0.1 mercurial=2.8.2-1ubuntu1.4 bzr=2.6.0+bzr6593-1ubuntu1.6 bzrtools=2.6.0-1 git-svn=1:1.9.1-1ubuntu0.10 -y )
#   libraries and headers
RUN (apt-get update ;apt-get install --no-install-recommends libcairo2-dev=1.13.0~20140204-0ubuntu1.1 libjpeg62-dev=6b1-4ubuntu1 libgif-dev=4.1.6-11 libpq-dev=9.3.24-0ubuntu0.14.04 libboost-all-dev=1.54.0.1ubuntu1 libzmq-dev=2.2.0+dfsg-5 libwww-curl-perl=4.15-1build3 libbz2-dev=1.0.6-5 dpkg-dev=1.17.5ubuntu5.8 ruby1.9.1-dev=1.9.3.484-2ubuntu1.14 dialog=1.2-20130928-1 python-gtk2=2.24.0-3ubuntu3 libpango1.0 libpango1.0-dev=1.36.3-1ubuntu1.1 libmysqlclient-dev=5.5.62-0ubuntu0.14.04.1 -y )
#   Enable repo for alternative python versions
RUN yes | add-apt-repository ppa:fkrull/deadsnakes
#   install ruby/rails/rvm
COPY ./files/etc/gemrc /etc/gemrc
RUN (apt-get update ;apt-get install --no-install-recommends gawk=1:4.0.1+dfsg-2.1ubuntu2 libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 libxslt-dev libgdbm-dev=1.8.3-12build1 libgmp-dev=2:5.1.3+dfsg-1ubuntu1 sqlite3=3.8.2-1ubuntu2.2 -y )
RUN sudo -u ubuntu -i bash -l -c " echo 'Installing rvm...' \
 && gpg -q --keyserver hkp://pgp.mit.edu --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3 \
 && (curl -sSL https://get.rvm.io | sudo bash -s stable --autolibs=enabled) " \
 && usermod -a -G rvm ubuntu
RUN sudo -u ubuntu -i bash -l -c " echo 'Installing stable ruby version...' \
 && rvm install ruby-2 \
 && rvm use ruby-2 --default"
#   Deployment
#   Can't include new npm since it makes "npm install -g npm" fail:
#   sudo -u ubuntu -i bash -l -c "npm install -g npm@3.3.6" && \
RUN wget -qO- https://toolbelt.heroku.com/install-ubuntu.sh | sh -e
RUN sudo -u ubuntu -i heroku --help
#   /opt packages
RUN curl https://storage.googleapis.com/golang/go1.8.3.linux-amd64.tar.gz | tar -C /opt -xz
RUN git clone https://github.com/lennartcl/gitl.git /opt/gitl
#   setup the system
RUN echo -n "" > /etc/motd \
 && echo "export PATH=/bin:/usr/bin:/sbin:/usr/sbin:/usr/local/bin:/usr/local/sbin" >> /root/.bashrc \
 && sudo -i -u ubuntu HOME=/home/ubuntu bash -i /etc/bash_completion \
 && HOME=/root bash -l /etc/bash_completion
#   Install apache
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.7-1ubuntu4.22 -y )
COPY ./files/etc /etc
COPY ./files/home /home
COPY ./files/gitignore /home/ubuntu/.gitignore
RUN cp /home/ubuntu/.gitignore /home/ubuntu/.hgignore \
 && mkdir -p /home/ubuntu/lib/apache2/lock /home/ubuntu/lib/apache2/log /home/ubuntu/lib/apache2/run \
 && cd /home/ubuntu \
 && mv ssh .ssh \
 && chmod 700 .ssh \
 && chown -R ubuntu: .bash* .gemrc lib .*ignore .ssh \
 && chown ubuntu: .
RUN echo "Configure apache2" \
 && a2enmod rewrite \
 && ln -s ../sites-available/001-cloud9.conf /etc/apache2/sites-enabled/001-cloud9.conf \
 && sed -i 's/Listen 80\b/Listen 8080/' /etc/apache2/ports.conf \
 && sed -Ei 's/(.*)secure_path.*/\1!secure_path/' /etc/sudoers \
 && touch /etc/init.d/systemd-logind
RUN echo "Configure php" \
 && /etc/init.d/mysql start \
 && /etc/init.d/postgresql start \
 && (apt-get update ;apt-get install --no-install-recommends php5=5.5.9+dfsg-1ubuntu4.29 php5-cli=5.5.9+dfsg-1ubuntu4.29 php5-fpm=5.5.9+dfsg-1ubuntu4.29 php5-pgsql=5.5.9+dfsg-1ubuntu4.29 php5-mysql=5.5.9+dfsg-1ubuntu4.29 phpmyadmin=4:4.0.10-1ubuntu0.1 phppgadmin=5.1-1 -y ) \
 && php5enmod c9 \
 && chmod 777 /var/lib/phpmyadmin/tmp \
 && cd /etc/php5/mods-available \
 && ls *.ini | sed 's/\.ini$//' | xargs php5enmod \
 && curl -sS https://getcomposer.org/installer | php \
 && mv composer.phar /usr/bin/composer
#   Add additional repos for users
#   MongoDB (Create the default data dir with correct permissions)
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10 \
 && echo "deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen" > /etc/apt/sources.list.d/mongodb.list \
 && : \
 && mkdir -p /data/db \
 && chown -R ubuntu:ubuntu /data
#   re-enable service startup. This line must come after all apt-get installs
RUN rm -f /usr/sbin/policy-rc.d
#   Set up /.check-environment folder
RUN mkdir -p /.check-environment
COPY ./files/check-environment /.check-environment/workspace
CMD /bin/bash -l
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
