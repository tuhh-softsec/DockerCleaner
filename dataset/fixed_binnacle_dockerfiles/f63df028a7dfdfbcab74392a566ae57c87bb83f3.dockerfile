FROM ubuntu:16.04
ENV workdir="/var/www"
#   Production OSM setup
ENV RAILS_ENV="production"
#   Postgres dependecies
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.17.1-1ubuntu1.5 -y )
RUN wget -q https://www.postgresql.org/media/keys/ACCC4CF8.asc -O - | apt-key add -
RUN sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt/ xenial-pgdg main" >> /etc/apt/sources.list.d/pgdg.list'
RUN :
#   Install the openstreetmap-website dependencies and passenger dependencies
RUN (apt-get update ;apt-get install --no-install-recommends ruby2.3=2.3.1-2~ubuntu16.04.16 libruby2.3=2.3.1-2~ubuntu16.04.16 ruby2.3-dev=2.3.1-2~ubuntu16.04.16 libmagickwand-dev=8:6.8.9.9-7ubuntu5.16 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxslt1-dev=1.1.28-2.1ubuntu0.3 apache2=2.4.18-2ubuntu3.17 apache2-dev=2.4.18-2ubuntu3.17 build-essential=12.1ubuntu2 git=1:2.7.4-0ubuntu1.10 postgresql=9.5+173ubuntu0.3 postgresql-contrib=9.5+173ubuntu0.3 libpq-dev=9.5.25-0ubuntu0.16.04.1 libsasl2-dev=2.1.26.dfsg1-14ubuntu0.2 imagemagick=8:6.8.9.9-7ubuntu5.16 libffi-dev=3.2.1-4 curl=7.47.0-1ubuntu2.19 -y )
RUN gem2.3 install bundler
#   Install node for some images process dependencies
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash -
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 -y )
#   Fixing image_optim issues, installing a bush of libraries from : https://github.com/toy/image_optim#pngout-installation-optional
RUN (apt-get update ;apt-get install --no-install-recommends advancecomp=1.20-1ubuntu0.2 gifsicle=1.88-1 jhead=1:3.00-4+deb9u1build0.16.04.1 jpegoptim=1.4.3-1 optipng=0.7.6-1ubuntu0.16.04.1 -y )
RUN git clone --recursive https://github.com/kornelski/pngquant.git \
 && cd pngquant \
 && ./configure \
 && make \
 && make install
RUN git clone https://github.com/tjko/jpeginfo.git \
 && cd jpeginfo \
 && ./configure \
 && make \
 && make strip \
 && make install
RUN wget http://iweb.dl.sourceforge.net/project/pmt/pngcrush/1.8.12/pngcrush-1.8.12.tar.gz \
 && tar zxf pngcrush-1.8.12.tar.gz \
 && cd pngcrush-1.8.12 \
 && make \
 && cp -f pngcrush /usr/local/bin
RUN npm install svgo@3.0.2 -g
#   Install openstreetmap-cgimap
RUN (apt-get update ;apt-get install --no-install-recommends libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libpqxx-dev=4.0.1+dfsg-3ubuntu2 libfcgi-dev=2.4.0-8.3 libboost-dev=1.58.0.1ubuntu1 libboost-regex-dev=1.58.0.1ubuntu1 libboost-program-options-dev=1.58.0.1ubuntu1 libboost-date-time-dev=1.58.0.1ubuntu1 libboost-filesystem-dev=1.58.0.1ubuntu1 libboost-system-dev=1.58.0.1ubuntu1 libboost-locale-dev=1.58.0.1ubuntu1 libmemcached-dev=1.0.18-4.1ubuntu2 libcrypto++-dev=5.6.1-9ubuntu0.1 automake=1:1.15-4ubuntu1 autoconf=2.69-9 libtool=2.4.6-0.1 libyajl-dev=2.1.0-2 -y )
ENV cgimap="/tmp/openstreetmap-cgimap"
RUN git clone git://github.com/zerebubuth/openstreetmap-cgimap.git $cgimap
RUN cd $cgimap \
 && ./autogen.sh \
 && ./configure \
 && make \
 && make install
#   Daemontools provides the `fghack` program required for running the `cgimap`
RUN (apt-get update ;apt-get install --no-install-recommends daemontools=1:0.76-6ubuntu1 -y )
#   Install the PGP key and add HTTPS support for APT
RUN (apt-get update ;apt-get install --no-install-recommends dirmngr=2.1.11-6ubuntu2.1 gnupg=1.4.20-1ubuntu3.3 -y )
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 561F9B9CAC40B2F7
RUN (apt-get update ;apt-get install --no-install-recommends apt-transport-https=1.2.35 ca-certificates=20210119~16.04.1 -y )
#   Add the APT repository
RUN sh -c 'echo deb https://oss-binaries.phusionpassenger.com/apt/passenger xenial main > /etc/apt/sources.list.d/passenger.list'
RUN :
#   Install Passenger + Apache module
RUN (apt-get update ;apt-get install --no-install-recommends libapache2-mod-passenger=5.0.27-2 -y )
#   Enable the Passenger Apache module and restart Apache
RUN echo "ServerName $( cat /etc/hostname ;)" >> /etc/apache2/apache2.conf
RUN a2enmod passenger
RUN apache2ctl restart
#   Check installation
RUN /usr/bin/passenger-config validate-install
RUN /usr/sbin/passenger-memory-stats
#   Clone the openstreetmap-website
RUN rm -rf $workdir
RUN git clone https://github.com/openstreetmap/openstreetmap-website.git $workdir
WORKDIR $workdir
#   gitsha 39f0b501e8cefea961447de9c8076e20fa3adbb4 at Jul 23, 2018
RUN git checkout 39f0b501e8cefea961447de9c8076e20fa3adbb4
#   Install the javascript runtime required by the `execjs` gem in
RUN (apt-get update ;apt-get install --no-install-recommends libv8-dev=3.14.5.8-5ubuntu2 -y )
RUN echo "gem 'therubyracer'" >> Gemfile
#   Install app dependencies
RUN bundle update listen \
 && bundle install
#   Configure database.yml, application.yml and secrets.yml
COPY config/database.yml $workdir/config/database.yml
COPY config/application.yml $workdir/config/application.yml
RUN echo "#session key \nproduction: \n secret_key_base: $( bundle exec rake secret ;)" > $workdir/config/secrets.yml
#   Protect sensitive information
RUN chmod 600 $workdir/config/database.yml $workdir/config/application.yml $workdir/config/secrets.yml
#   Configure ActionMailer SMTP settings, Replace config/initializers/action_mailer.rb with out configurations
COPY config/action_mailer.rb config/initializers/action_mailer.rb
#   Precompile the website assets
RUN bundle exec rake assets:precompile --trace
#   The rack interface requires a `tmp` directory to use openstreetmap-cgimap
RUN ln -s /tmp /var/www/tmp
#   Add Apache configuration file
COPY config/production.conf /etc/apache2/sites-available/production.conf
RUN a2dissite 000-default
RUN a2ensite production
#   Enable required apache modules for the cgimap Apache service
RUN a2enmod proxy proxy_http rewrite
#   Config the virtual host apache2
COPY config/cgimap.conf /tmp/
RUN sed -e 's/RewriteRule ^(.*)/#RewriteRule ^(.*)/' -e 's/\/var\/www/\/var\/www\/public/g' /tmp/cgimap.conf > /etc/apache2/sites-available/cgimap.conf
RUN chmod 644 /etc/apache2/sites-available/cgimap.conf
RUN a2ensite cgimap
RUN apache2ctl configtest
#   Set Permissions for www-data
RUN chown -R www-data: /var/www
#   Script to start the app
COPY start.sh $workdir/start.sh
CMD $workdir/start.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
