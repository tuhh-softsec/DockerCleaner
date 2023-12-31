#   Based on Code.org CircleCI-dependencies Dockerfile in .circle directory
#   Pushed to Docker Hub at wintercdo/code-dot-org:0.7
FROM ubuntu:14.04
USER root
#   set timezone to UTC by default
RUN ln -sf /usr/share/zoneinfo/Etc/UTC /etc/localtime
#   use unicode
RUN locale-gen C.UTF-8 || true
ENV LANG="C.UTF-8"
#   add circleci user
RUN groupadd --gid 3434 circleci \
 && useradd --uid 3434 --gid circleci --shell /bin/bash --create-home circleci \
 && echo 'circleci ALL=NOPASSWD: ALL' >> /etc/sudoers.d/50-circleci
#   essential tools
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends curl=7.35.0-1ubuntu2.20 apt-transport-https=1.0.1ubuntu2.24 -y )
#   add yarn
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
 && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
#   more tools
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 xvfb=2:1.15.1-0ubuntu2.11 yarn=1.6.0-1 sudo=1.8.9p5-1ubuntu1.4 openssh-client=1:6.6p1-2ubuntu2.13 ca-certificates=20170717~14.04.2 tar=1.27.1-1ubuntu0.1 gzip=1.6-3ubuntu1 wget=1.15-1ubuntu1.14.04.5 xz-utils=5.1.1alpha+20120614-2ubuntu2 autoconf=2.69-6 build-essential=11.6ubuntu6 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 libssl-dev=1.0.1f-1ubuntu2.27 curl=7.35.0-1ubuntu2.20 libreadline-dev=6.3-4ubuntu2 python=2.7.5-5ubuntu3 python-dev=2.7.5-5ubuntu3 -y )
#   install node
RUN wget https://nodejs.org/dist/v8.15.0/node-v8.15.0.tar.gz \
 && tar -xzvf node-v8.15.0.tar.gz \
 && rm node-v8.15.0.tar.gz \
 && cd node-v8.15.0 \
 && ./configure \
 && make -j4 \
 && make install \
 && cd .. \
 && rm -r node-v8.15.0
#   more more tools
RUN (apt-get update ;apt-get install --no-install-recommends unzip=6.0-9ubuntu1.5 -y )
#   install firefox
RUN curl --output /tmp/firefox.deb https://s3.amazonaws.com/circle-downloads/firefox-mozilla-build_47.0.1-0ubuntu1_amd64.deb \
 && echo 'ef016febe5ec4eaf7d455a34579834bcde7703cb0818c80044f4d148df8473bb /tmp/firefox.deb' | sha256sum -c \
 && sudo dpkg -i /tmp/firefox.deb || sudo apt-get -f install \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libgtk3.0-cil-dev=2.99.2-2ubuntu1 -y ) \
 && rm -rf /tmp/firefox.deb
#   install chrome
RUN curl -sSL -o /tmp/google-chrome-stable_current_amd64.deb https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
 && (sudo dpkg -i /tmp/google-chrome-stable_current_amd64.deb || sudo apt-get -fy install ) \
 && rm -rf /tmp/google-chrome-stable_current_amd64.deb \
 && sudo sed -i 's|HERE/chrome"|HERE/chrome" --disable-setuid-sandbox --no-sandbox|g' "/opt/google/chrome/google-chrome"
#   install chromedriver
RUN export CHROMEDRIVER_RELEASE=$( curl http://chromedriver.storage.googleapis.com/LATEST_RELEASE ;) \
 && curl -sSL -o /tmp/chromedriver_linux64.zip "http://chromedriver.storage.googleapis.com/$CHROMEDRIVER_RELEASE/chromedriver_linux64.zip" \
 && cd /tmp \
 && unzip chromedriver_linux64.zip \
 && rm -rf chromedriver_linux64.zip \
 && sudo mv chromedriver /usr/local/bin/chromedriver \
 && sudo chmod +x /usr/local/bin/chromedriver
#   install mysql
RUN curl -sSL -o /tmp/mysql-apt-config_0.8.9-1_all.deb https://dev.mysql.com/get/mysql-apt-config_0.8.9-1_all.deb \
 && echo "mysql-apt-config mysql-apt-config/select-server select mysql-5.7" | /usr/bin/debconf-set-selections \
 && DEBIAN_FRONTEND=noninteractive dpkg -i /tmp/mysql-apt-config_0.8.9-1_all.deb || (apt-get update ;apt-get install --no-install-recommends -fy ) \
 && rm -rf /tmp/mysql-apt-config_0.8.9-1_all.deb \
 && apt-get update \
 && DEBIAN_FRONTEND=noninteractive apt-get install -y mysql-server libmysqlclient-dev
RUN service mysql start \
 && echo "ALTER USER 'root'@'localhost' IDENTIFIED WITH mysql_native_password BY '';" | mysql \
 && service mysql stop
#   install a couple more things from config.yml
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends parallel=20161222-1~ubuntu0.14.04.2 libmagickwand-dev=8:6.7.7.10-6ubuntu3.13 imagemagick=8:6.7.7.10-6ubuntu3.13 -y )
RUN mv /usr/bin/parallel /usr/bin/gnu_parallel
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends libicu-dev=52.1-3ubuntu0.8 enscript=1.6.5.90-2 moreutils=0.50 pdftk=2.01-1 libmysqlclient-dev=5.5.62-0ubuntu0.14.04.1 libsqlite3-dev=3.8.2-1ubuntu2.2 -y )
RUN wget https://github.com/htacg/tidy-html5/releases/download/5.4.0/tidy-5.4.0-64bit.deb \
 && dpkg -i tidy-5.4.0-64bit.deb \
 && rm tidy-5.4.0-64bit.deb
RUN mv /usr/bin/gnu_parallel /usr/bin/parallel
RUN (apt-get update ;apt-get install --no-install-recommends rbenv=0.4.0+debian1-2 -y )
#   install https://github.com/boxboat/fixuid
RUN USER=circleci \
 && GROUP=circleci \
 && curl -SsL https://github.com/boxboat/fixuid/releases/download/v0.4/fixuid-0.4-linux-amd64.tar.gz | tar -C /usr/local/bin -xzf - \
 && chown root:root /usr/local/bin/fixuid \
 && chmod 4755 /usr/local/bin/fixuid \
 && mkdir -p /etc/fixuid \
 && printf "user: $USER\ngroup: $GROUP\n" > /etc/fixuid/config.yml
USER circleci
#   Install ruby-build: https://github.com/rbenv/ruby-build#readme
RUN mkdir -p "$( rbenv root ;)"/plugins
RUN git clone https://github.com/rbenv/ruby-build.git "$( rbenv root ;)"/plugins/ruby-build
RUN rbenv install 2.5.0
RUN eval "$( rbenv init - ;)" \
 && rbenv global 2.5.0 \
 && rbenv rehash \
 && gem install bundler --version 1.17
#   This bashrc file will be used whenever someone runs bash in interactive mode.
#   This is mostly intended for the use case where you want to start a shell into a running container with
#   docker exec -it <container_name> bash, which bypasses the entrypoint script.
RUN echo 'eval "$(rbenv init -)"' >> ~/.bashrc
#   We need git >= 2.15 to use git rev-parse --is-shallow-clone feature
#   TODO: consolidate apt-get installs
RUN sudo apt-get install -y software-properties-common \
 && sudo add-apt-repository ppa:git-core/ppa \
 && sudo apt-get update \
 && sudo apt-get install -y git
RUN sudo apt-get install -y jq
#   en_US.UTF-8 locale not available by default
RUN sudo locale-gen en_US.UTF-8
COPY entrypoint.sh /entrypoint.sh
RUN sudo chmod +x /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
# Please add your HEALTHCHECK here!!!
