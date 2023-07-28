#   Dockie Development Environment
FROM ubuntu:15.10
MAINTAINER Rob Loach <robloach@gmail.com>
#   Update
ENV DEBIAN_FRONTEND="noninteractive"
RUN : \
 && apt-get upgrade -y
#   Supervisor
RUN (apt-get update ;apt-get install --no-install-recommends supervisor -y ) \
 && mkdir -p /var/log/supervisor \
 && supervisord --version
COPY configs/supervisor/supervisor.conf /etc/supervisor/conf.d/supervisor.conf
#   Base Dependencies
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common wget mysql-client unzip postfix git mercurial subversion iputils-ping -y ) \
 && git --version \
 && hg --version \
 && svn --version
#   Zsh
RUN (apt-get update ;apt-get install --no-install-recommends zsh -y ) \
 && git clone git://github.com/robbyrussell/oh-my-zsh.git /root/.oh-my-zsh \
 && chsh --shell $( which zsh ;) \
 && zsh --version
COPY configs/oh-my-zsh/zshrc /root/.zshrc
#   PHP
RUN (apt-get update ;apt-get install --no-install-recommends php5-apcu php-pear php5-cgi php5-json php5-cli php5-curl curl php5-mcrypt php5-xdebug mcrypt libmcrypt-dev php5-mysql php5-gd php5-sqlite sqlite imagemagick php5-imagick -y ) \
 && php5enmod mcrypt pdo pdo_sqlite sqlite3 \
 && php --version
COPY configs/php/php-cli.ini /etc/php5/cli/conf.d/dockie-dockie.ini
#   Composer
ENV COMPOSER_HOME="/root/.composer"
ENV PATH="$COMPOSER_HOME/vendor/bin:$PATH"
RUN curl -sS https://getcomposer.org/installer | php -- --filename=composer --install-dir=/usr/bin --version=1.0.0-alpha10
RUN composer --version
#   PHPUnit
RUN composer global require phpunit/phpunit:5.* \
 && phpunit --version
#   Drush
RUN composer global require drush/drush:8.* \
 && drush --version
#   Python
RUN (apt-get update ;apt-get install --no-install-recommends python python3 python-pil pylint -y ) \
 && python --version \
 && python3 --version
#   Node.js
RUN (apt-get update ;apt-get install --no-install-recommends build-essential -y ) \
 && curl -sL https://deb.nodesource.com/setup_4.x | sh -
RUN apt-get update --fix-missing \
 && (apt-get update ;apt-get install --no-install-recommends nodejs -y ) \
 && nodejs --version \
 && npm --version \
 && npm install coffee-script@1.12.7 bower@1.8.14 grunt-cli@1.4.3 gulp@4.0.2 component@1.1.0 yo@4.3.1 eslint@8.38.0 -g
#   Ruby
RUN (apt-get update ;apt-get install --no-install-recommends ruby ruby-dev ri -y ) \
 && ruby --version \
 && echo "Gem version:" \
 && gem --version
RUN gem install rake --version 13.0.6 \
 && bundle --version \
 && rake --version \
 && sass --version \
 && gem install compass --version 1.0.3 \
 && compass --version
#   Go
RUN (apt-get update ;apt-get install --no-install-recommends golang -y ) \
 && go version
#   Rust
RUN mkdir -p /tmp/rust \
 && cd /tmp/rust \
 && wget https://static.rust-lang.org/dist/rust-1.3.0-x86_64-unknown-linux-gnu.tar.gz
RUN cd /tmp/rust \
 && tar -zxvf * \
 && cd r* \
 && ./install.sh \
 && rm -rf /tmp/rust
RUN rustc --version
RUN cargo --version
#   Version Check
COPY configs/dockie/dockie-version /dockie-version
RUN chmod +x /dockie-version
#   Clean up
RUN apt-get clean \
 && apt-get autoremove
RUN /dockie-version
#   Start
VOLUME /app
EXPOSE 22/tcp
CMD ["supervisord"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
