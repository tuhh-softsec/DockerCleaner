FROM ubuntu:16.04
LABEL maintainer="John Chilton <jmchilton@gmail.com>"
ARG CHROME_VERSION="google-chrome-beta"
ARG CHROME_DRIVER_VERSION="2.38"
#   TODO: merge with first ENV statement.
ENV DEBIAN_FRONTEND="noninteractive" \
    DEBCONF_NONINTERACTIVE_SEEN="true" \
    MYSQL_MAJOR="5.7" \
    POSTGRES_MAJOR="9.5" \
    GALAXY_ROOT="/galaxy" \
    GALAXY_VIRTUAL_ENV="/galaxy_venv" \
    GALAXY_VIRTUAL_ENV_2="/galaxy_venv" \
    GALAXY_VIRTUAL_ENV_3="/galaxy_venv3" \
    LC_ALL="C.UTF-8"
#   Pre-install a bunch of packages to speed up ansible steps.
RUN apt-get update -y \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 apt-transport-https=1.2.35 curl=7.47.0-1ubuntu2.19 -y \
 && apt-add-repository -y ppa:ansible/ansible \
 && curl -s https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list \
 && curl -s http://neuro.debian.net/lists/xenial.us-ca.full > /etc/apt/sources.list.d/neurodebian.sources.list \
 && apt-key adv --recv-keys --keyserver hkp://pool.sks-keyservers.net:80 0xA5D32F012649A5A9 \
 && apt-get update -y \
 && apt-get install --no-install-recommends postgresql=9.5+173ubuntu0.3 postgresql-client=9.5+173ubuntu0.3 ansible=2.0.0.2-2ubuntu1.3 wget=1.17.1-1ubuntu1.5 python3-dev=3.5.1-3 git-core=1:2.7.4-0ubuntu1.10 python-prettytable=0.7.2-3 python-virtualenv=15.0.1+ds-3ubuntu1.1 python-pip=8.1.1-2ubuntu0.6 rsync=3.1.1-3ubuntu1.3 swig=3.0.8-0ubuntu3 sysstat=11.2.0-1ubuntu0.3 unzip=6.0-20ubuntu1.1 openssl=1.0.2g-1ubuntu4.20 bzip2=1.0.6-8ubuntu0.2 ca-certificates=20210119~16.04.1 openjdk-8-jre-headless=8u292-b10-0ubuntu1~16.04.1 tzdata=2021a-0ubuntu0.16.04 sudo=1.8.16-0ubuntu1.10 locales=2.23-0ubuntu11.3 xvfb=2:1.18.4-0ubuntu0.12 ffmpeg=7:2.8.17-0ubuntu0.1 bcftools=1.2-2 singularity-container libnss3=2:3.28.4-0ubuntu0.16.04.14 libgconf-2-4=3.2.6-3ubuntu6 ${CHROME_VERSION:-google-chrome-stable} -y \
 && apt-get autoremove -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN mkdir -p /tmp/ansible \
 && mkdir -p /opt/galaxy/db \
 && chown -R postgres:postgres /opt/galaxy/db
COPY ansible_vars.yml /tmp/ansible/ansible_vars.yml
COPY provision.yml /tmp/ansible/provision.yml
RUN mkdir /etc/galaxy \
 && cd /tmp/ansible \
 && mkdir roles \
 && mkdir roles/galaxyprojectdotorg.galaxy-os \
 && wget --quiet -O- https://github.com/galaxyproject/ansible-galaxy-os/archive/master.tar.gz | tar -xzf- --strip-components=1 -C roles/galaxyprojectdotorg.galaxy-os \
 && mkdir roles/galaxyprojectdotorg.cloudman-database \
 && wget --quiet -O- https://github.com/galaxyproject/ansible-cloudman-database/archive/master.tar.gz | tar -xzf- --strip-components=1 -C roles/galaxyprojectdotorg.cloudman-database \
 && mkdir roles/galaxyprojectdotorg.galaxy \
 && wget --quiet -O- https://github.com/galaxyproject/ansible-galaxy/archive/master.tar.gz | tar -xzf- --strip-components=1 -C roles/galaxyprojectdotorg.galaxy \
 && mkdir roles/galaxyprojectdotorg.galaxy-extras \
 && wget --quiet -O- https://github.com/galaxyproject/ansible-galaxy-extras/archive/dynamic_uwsgi_config.tar.gz | tar -xzf- --strip-components=1 -C roles/galaxyprojectdotorg.galaxy-extras \
 && mkdir roles/galaxyprojectdotorg.galaxy-toolshed \
 && wget --quiet -O- https://github.com/galaxyproject/ansible-galaxy-toolshed/archive/master.tar.gz | tar -xzf- --strip-components=1 -C roles/galaxyprojectdotorg.galaxy-toolshed \
 && ANSIBLE_FORCE_COLOR=1 PYTHONUNBUFFERED=1 ansible-playbook /tmp/ansible/provision.yml --tags=image -c local -e "@ansible_vars.yml" \
 && ANSIBLE_FORCE_COLOR=1 PYTHONUNBUFFERED=1 ansible-playbook /tmp/ansible/provision.yml --tags=database -c local -e "@ansible_vars.yml" \
 && ANSIBLE_FORCE_COLOR=1 PYTHONUNBUFFERED=1 ansible-playbook /tmp/ansible/provision.yml --tags=galaxy -c local -e "@ansible_vars.yml" \
 && ANSIBLE_FORCE_COLOR=1 PYTHONUNBUFFERED=1 ansible-playbook /tmp/ansible/provision.yml --tags=toolshed -c local -e "@ansible_vars.yml" \
 && apt-get autoremove -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN cd $GALAXY_ROOT \
 && virtualenv -p /usr/bin/python3 $GALAXY_VIRTUAL_ENV_3 \
 && for VENV in $GALAXY_VIRTUAL_ENV_2 $GALAXY_VIRTUAL_ENV_3; do export GALAXY_VIRTUAL_ENV=$VENV \
 && ./scripts/common_startup.sh \
 && dev_requirements=./lib/galaxy/dependencies/dev-requirements.txt \
 && [ -f $dev_requirements ] \
 && $VENV/bin/pip install -r $dev_requirements ; done
RUN for VENV in $GALAXY_VIRTUAL_ENV_3 $GALAXY_VIRTUAL_ENV_2; do export GALAXY_VIRTUAL_ENV=$VENV \
 && . $GALAXY_VIRTUAL_ENV/bin/activate \
 && pip install psycopg2-binary==2.9.6 ; done \
 && cd $GALAXY_ROOT \
 && echo "Prepopulating postgres database" \
 && su -c '/usr/lib/postgresql/${POSTGRES_MAJOR}/bin/pg_ctl -o "-F" start -D /opt/galaxy/db' postgres \
 && sleep 3 \
 && GALAXY_CONFIG_DATABASE_CONNECTION="postgresql://root@localhost:5930/galaxy" bash create_db.sh \
 && echo "Prepopulating sqlite database" \
 && GALAXY_CONFIG_DATABASE_CONNECTION="sqlite:////opt/galaxy/galaxy.sqlite" bash create_db.sh \
 && echo "Prepopulating toolshed postgres database" \
 && TOOL_SHED_CONFIG_DATABASE_CONNECTION="postgresql://root@localhost:5930/toolshed" bash create_db.sh tool_shed \
 && echo "Prepopulating toolshed sqlite database" \
 && TOOL_SHED_CONFIG_DATABASE_CONNECTION="sqlite:////opt/galaxy/toolshed.sqlite" bash create_db.sh tool_shed
#  ========================================
#   Add Selenium user with passwordless sudo
#  ========================================
RUN useradd seluser --shell /bin/bash --create-home \
 && usermod -a -G sudo seluser \
 && echo 'ALL ALL = (ALL) NOPASSWD: ALL' >> /etc/sudoers \
 && echo 'seluser:secret' | chpasswd
USER seluser
#  ==========
#   Selenium
#  ==========
RUN sudo mkdir -p /opt/selenium \
 && sudo chown seluser:seluser /opt/selenium \
 && wget --no-verbose https://selenium-release.storage.googleapis.com/3.6/selenium-server-standalone-3.6.0.jar -O /opt/selenium/selenium-server-standalone.jar
USER root
#  ==============================
#   Scripts to run Selenium Node
#  ==============================
COPY selenium/entry_point.sh selenium/functions.sh selenium/wrap_chrome_binary selenium/generate_config /opt/bin/
RUN /opt/bin/wrap_chrome_binary
USER seluser
RUN CD_VERSION=$( if [ ${CHROME_DRIVER_VERSION:-latest} = "latest" ] ; then echo $( wget -qO- https://chromedriver.storage.googleapis.com/LATEST_RELEASE ;) ; else echo $CHROME_DRIVER_VERSION ; fi ;) \
 && echo "Using chromedriver version: "$CD_VERSION \
 && wget --no-verbose -O /tmp/chromedriver_linux64.zip https://chromedriver.storage.googleapis.com/$CD_VERSION/chromedriver_linux64.zip \
 && rm -rf /opt/selenium/chromedriver \
 && unzip /tmp/chromedriver_linux64.zip -d /opt/selenium \
 && rm /tmp/chromedriver_linux64.zip \
 && mv /opt/selenium/chromedriver /opt/selenium/chromedriver-$CD_VERSION \
 && chmod 755 /opt/selenium/chromedriver-$CD_VERSION \
 && sudo ln -fs /opt/selenium/chromedriver-$CD_VERSION /usr/bin/chromedriver
RUN /opt/bin/generate_config > /opt/selenium/config.json
#  ============================
#   Some configuration options
#  ============================
ENV SCREEN_WIDTH="1360" \
    SCREEN_HEIGHT="1020" \
    SCREEN_DEPTH="24" \
    DISPLAY=":99.0" \
    NODE_MAX_INSTANCES="1" \
    NODE_MAX_SESSION="1" \
    NODE_PORT="5555" \
    NODE_REGISTER_CYCLE="5000" \
    NODE_POLLING="5000" \
    NODE_UNREGISTER_IF_STILL_DOWN_AFTER="60000" \
    NODE_DOWN_POLLING_LIMIT="2" \
    NODE_APPLICATION_NAME="" \
    DBUS_SESSION_BUS_ADDRESS="/dev/null"
USER root
COPY run_test_wrapper.sh /usr/local/bin/run_test_wrapper.sh
EXPOSE 9009/tcp
EXPOSE 8080/tcp
EXPOSE 80/tcp
ENTRYPOINT ["/bin/bash", "/usr/local/bin/run_test_wrapper.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
