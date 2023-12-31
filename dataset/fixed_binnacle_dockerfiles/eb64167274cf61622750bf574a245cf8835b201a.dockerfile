FROM prevsio/pool-base
MAINTAINER mookjp
WORKDIR /tmp
RUN export PATH=$PATH:/usr/local/bin
#   Update docker
RUN curl -s https://get.docker.com/builds/Linux/x86_64/docker-latest -o docker
RUN chmod +x docker
RUN cp docker /usr/bin/
#   hostname settings
COPY provisioning/network /etc/sysconfig/network
COPY provisioning/hosts /etc/sysconfig/hosts
#   Add Apache settings
COPY provisioning/httpd.conf /etc/httpd/conf/httpd.conf
COPY provisioning/mruby.conf /etc/httpd/conf.d/mruby.conf
#   Add supervisor configuration file
COPY provisioning/supervisord.conf /etc/supervisord.conf
#   Add container-limitation configuration gile
COPY provisioning/cron/limit_containers /etc/cron.d/limit_containers
#   Install build-srver
COPY builder /tmp/builder
WORKDIR /tmp/builder
RUN /opt/ruby-2.1.2/bin/bundle install --path=vendor/bundle
#   Test builder
RUN /opt/ruby-2.1.2/bin/bundle exec rake spec
RUN /usr/local/bin/gem build builder.gemspec
RUN /usr/local/bin/gem install builder-0.0.1.gem
#   Add mod_mruby handler to manage request
COPY handlers /app/handlers
#   Install build-screen
RUN mkdir -p /app/handlers/resources
RUN mkdir /tmp/build-screen
#   Add package.json beforehand then execute npm install
COPY build-screen/package.json /tmp/build-screen/package.json
WORKDIR /tmp/build-screen
RUN npm install
COPY build-screen /tmp/build-screen-2
RUN cp -nr /tmp/build-screen-2/* /tmp/build-screen
RUN cp -nr /tmp/build-screen-2/.[^.]* /tmp/build-screen
#   For bower install
RUN git config --global url.https://.insteadOf git://
RUN $( npm bin ;)/bower --allow-root install
RUN $( npm bin ;)/grunt build
RUN mv /tmp/build-screen/dist /app/handlers/resources/build-screen
#   Add util scripts for handling containers
COPY scripts /app/scripts
RUN chmod +x /app/scripts/starter
RUN chmod +x /app/scripts/limit_containers
#   Add log directories
RUN mkdir -p /var/log/supervisor
RUN mkdir -p /var/log/builder
RUN mkdir -p /app/images
RUN touch /app/images/ids
#   Add private key directory to clone repository
RUN mkdir -p /root/.ssh
RUN mkdir -p /var/www/.ssh
COPY keys /app/keys
COPY /provisioning/ssh_config /var/www/.ssh/config
COPY /provisioning/ssh_config /root/.ssh/config
RUN chown -R apache. /var/www/.ssh
RUN chmod 600 /var/www/.ssh/config /root/.ssh/config
RUN chmod 700 /var/www/.ssh /root/.ssh
#   Add config files
COPY config /app/config
#   Add test files
COPY tests /app/tests
#   Add apache to pool and docker group to access /app
#   as hook.rb has to access application's repository in it
#   and to access docker sock file
RUN groupadd pool
RUN usermod -G docker,pool apache
RUN chgrp --recursive pool /app
RUN chmod --recursive g+rwx /app
#   Set target preview repository
ENV PREVIEW_REPOSITORY_URL="https://github.com/mookjp/flaskapp.git"
ENV MAX_CONTAINERS="10"
ENV GIT_COMMIT_ID_CACHE_EXPIRE="10"
ENV POOL_BASE_DOMAIN="pool.dev"
ENV GITHUB_BOT="false"
EXPOSE 80/tcp 8080/tcp
CMD /app/scripts/starter \
 && tail -F /var/log/httpd/error_log
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
