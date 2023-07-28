#   DOCKER-VERSION 1.2.0
FROM ubuntu:14.04
MAINTAINER Seyhun Akyurek "seyhunak@gmail.com"
#   Install dependency packages
RUN :
RUN apt-get upgrade -y
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.92.37.8 -y )
RUN (apt-get update ;apt-get install --no-install-recommends make=3.81-8.2ubuntu3 gcc=4:4.8.2-1ubuntu6 wget=1.15-1ubuntu1.14.04.5 openjdk-6-jre=6b41-1.13.13-0ubuntu0.14.04.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 -y )
RUN apt-get clean
#   Deploy rbenv and ruby-build
RUN git clone https://github.com/sstephenson/rbenv.git /root/.rbenv
RUN git clone https://github.com/sstephenson/ruby-build.git /root/.rbenv/plugins/ruby-build
ENV RBENV_ROOT="/root/.rbenv"
ENV PATH="/root/.rbenv/bin:/root/.rbenv/shims:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
COPY ./rbenv.sh /etc/profile.d/rbenv.sh
#   Install Ruby
RUN rbenv install 2.1.2
RUN rbenv global 2.1.2
RUN rbenv rehash
#   Install Bundler
RUN gem install bundler --version 2.4.12 --no-ri --no-rdoc
#   Install Rails
RUN gem install rails-4.2.0.beta1 --version unknown
#   Install Curl and Node.js (for asset pipeline)
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.35.0-1ubuntu2.20 -qq -y )
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=0.10.25~dfsg2-2ubuntu1.2 -qq -y )
#   Install nginx
RUN add-apt-repository -y ppa:nginx/stable
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends nginx=1.4.6-1ubuntu3.9 -qq -y )
RUN echo "daemon off;" >> /etc/nginx/nginx.conf
RUN chown -R www-data:www-data /var/lib/nginx
COPY nginx_sites.conf /etc/nginx/sites-enabled/default
#   Publish port 80
EXPOSE 80/tcp
#   Start nginx when container starts
ENTRYPOINT /usr/sbin/nginx
#   Install foreman
RUN gem install foreman --version 0.87.2
#   Add default foreman config
COPY Procfile /home/rails/Procfile
#   Install Unicorn
RUN gem install unicorn --version 6.1.0
#   Add default unicorn config
COPY unicorn.rb /home/rails/config/unicorn.rb
#   Install Beanstalkd
RUN (apt-get update ;apt-get install --no-install-recommends beanstalkd=1.9-2ubuntu1 -qq -y )
#   Install backburner
RUN gem install backburner --version 1.6.1
#   Add default unicorn config
COPY backburner.rb /home/rails/config/initializers/backburner.rb
#   Install Solr
RUN wget http://archive.apache.org/dist/lucene/solr/3.6.2/apache-solr-3.6.2.tgz -O /tmp/pkg.tar.gz
RUN (cd /tmp \
 && tar zxf pkg.tar.gz \
 && mv apache-solr-* /opt/solr )
RUN rm -rf /tmp/*
COPY run.sh /usr/local/bin/run
RUN chmod +x /usr/local/bin/run
EXPOSE 8983/tcp
CMD ["/usr/local/bin/run"]
#   Install Solr
RUN gem install sunspot --version 2.6.0
RUN gem install sunspot_solr --version 2.6.0
RUN gem install sunspot_rails --version 2.6.0
#   Install MySQL (for mysql, mysql2 gem)
RUN (apt-get update ;apt-get install --no-install-recommends libmysqlclient-dev=5.5.62-0ubuntu0.14.04.1 -qq -y )
RUN gem install mysql2 --version 0.5.5
#   Install Redis
RUN (apt-get update ;apt-get install --no-install-recommends python-pip=1.5.4-1ubuntu4 redis-server=2:2.8.4-2ubuntu0.2 -qq -y )
#   Setting up Rails app
WORKDIR /home/rails
ONBUILD ADD Gemfile /home/rails/Gemfile
ONBUILD ADD Gemfile.lock /home/rails/Gemfile.lock
ONBUILD RUN bundle install --without development test
ONBUILD ADD . /home/rails
#   Set ENV Variables
ENV RAILS_ENV="production"
CMD bundle exec rake assets:precompile \
 && foreman start -f Procfile
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
