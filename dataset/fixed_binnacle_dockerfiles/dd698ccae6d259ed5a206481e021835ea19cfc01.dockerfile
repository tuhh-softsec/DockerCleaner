FROM ubuntu
ENV DEBIAN_FRONTEND="noninteractive"
RUN dpkg-divert --local --rename --add /sbin/initctl
RUN ln -s /bin/true /sbin/initctl
RUN apt-get update ; (apt-get update ;apt-get install --no-install-recommends lsb-release=12.0-1ubuntu1 software-properties-common=0.99.35 -y )
#   add sources
RUN add-apt-repository -y ppa:git-core/ppa ; echo deb http://us.archive.ubuntu.com/ubuntu/ $( lsb_release -cs ;) universe multiverse >> /etc/apt/sources.list; echo deb http://us.archive.ubuntu.com/ubuntu/ $( lsb_release -cs ;)-updates main restricted universe >> /etc/apt/sources.list; echo deb http://security.ubuntu.com/ubuntu $( lsb_release -cs ;)-security main restricted universe >> /etc/apt/sources.list
#   run upgrades
RUN echo udev hold | dpkg --set-selections ; echo initscripts hold | dpkg --set-selections ; echo upstart hold | dpkg --set-selections ; : ; apt-get -y upgrade
#   install dependencies
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=12.9ubuntu3 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 libyaml-dev=0.2.5-1 libssl-dev=3.0.8-1ubuntu1 libgdbm-dev=1.23-3 libreadline-dev=8.2-1.3 libncurses5-dev=6.4-2 libffi-dev=3.4.4-1 curl=7.88.1-7ubuntu1 git-core openssh-server=1:9.0p1-1ubuntu8 redis-server=5:7.0.8-4 checkinstall=1.6.2+git20170426.d24a630-3ubuntu1 libxml2-dev=2.9.14+dfsg-1.1build2 libxslt-dev libcurl4-openssl-dev=7.88.1-7ubuntu1 libicu-dev=72.1-3ubuntu1 logrotate=3.21.0-1 libpq-dev=15.2-1 sudo=1.9.13p1-1ubuntu2 git=1:2.39.2-1ubuntu1 -y )
#   install Ruby
RUN mkdir /tmp/ruby ; cd /tmp/ruby ; curl ftp://ftp.ruby-lang.org/pub/ruby/2.0/ruby-2.0.0-p247.tar.gz | tar xz ; cd ruby-2.0.0-p247 ; chmod +x configure ; ./configure ; make ; make install ; gem install bundler --version 2.4.12 --no-ri --no-rdoc
#   create git user
RUN adduser --disabled-login --gecos 'GitLab' git
#   install gitlab shell
RUN cd /home/git ; su git -c "git clone https://github.com/gitlabhq/gitlab-shell.git" ; cd gitlab-shell ; su git -c "git checkout v1.7.6" ; su git -c "cp config.yml.example config.yml" ; sed -i -e 's/localhost/127.0.0.1:8080/g' config.yml ; su git -c "./bin/install"
#   install Gitlab
RUN cd /home/git ; su git -c "git clone https://github.com/gitlabhq/gitlabhq.git gitlab" ; cd /home/git/gitlab ; su git -c "git checkout 6-2-stable"
#   misc configuration stuff
RUN cd /home/git/gitlab ; chown -R git tmp/ ; chown -R git log/ ; chmod -R u+rwX log/ ; chmod -R u+rwX tmp/ ; su git -c "mkdir /home/git/gitlab-satellites" ; su git -c "mkdir tmp/pids/" ; su git -c "mkdir tmp/sockets/" ; chmod -R u+rwX tmp/pids/ ; chmod -R u+rwX tmp/sockets/ ; su git -c "mkdir public/uploads" ; chmod -R u+rwX public/uploads ; su git -c "cp config/unicorn.rb.example config/unicorn.rb" ; su git -c "cp config/initializers/rack_attack.rb.example config/initializers/rack_attack.rb" ; su git -c 'sed -ie "s/# config.middleware.use Rack::Attack/config.middleware.use Rack::Attack/" config/application.rb' ; su git -c "git config --global user.name 'GitLab'" ; su git -c "git config --global user.email 'gitlab@localhost'" ; su git -c "git config --global core.autocrlf input"
RUN cd /home/git/gitlab ; gem install charlock_holmes --version 0.7.7 ; su git -c "bundle install --deployment --without development test mysql aws"
#   install init scripts
RUN cd /home/git/gitlab ; cp lib/support/init.d/gitlab /etc/init.d/gitlab ; chmod +x /etc/init.d/gitlab ; update-rc.d gitlab defaults 21
RUN cd /home/git/gitlab ; cp lib/support/logrotate/gitlab /etc/logrotate.d/gitlab
#   postgresql
RUN (apt-get update ;apt-get install --no-install-recommends postgresql=15+248 postgresql-client=15+248 -y )
RUN ln -s /usr/lib/postgresql/9.1/bin/postgres /usr/local/bin/
#   nginx
RUN (apt-get update ;apt-get install --no-install-recommends nginx=1.22.0-1ubuntu3 -y )
#   make config files available to container
COPY . /srv/gitlab
#   make scripts executable
RUN chmod +x /srv/gitlab/start.sh ; chmod +x /srv/gitlab/firstrun.sh
#   more setup is done in firstrun script
RUN /srv/gitlab/firstrun.sh
#   expose http, https and ssh
EXPOSE 80/tcp
EXPOSE 443/tcp
EXPOSE 22/tcp
#   when docker is invoked with 'docker run', start this command by default
CMD ["/srv/gitlab/start.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
