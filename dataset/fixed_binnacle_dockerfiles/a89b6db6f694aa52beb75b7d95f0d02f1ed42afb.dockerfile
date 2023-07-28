#   Copyright 2015 Google Inc. All Rights Reserved.
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
FROM debian:jessie
#   Install the dependencies.
RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends mysql-client libmysqlclient-dev apache2 php5 libapache2-mod-php5 php5-mcrypt php5-mysql php5-gd php5-dev php5-curl php-apc php5-cli php5-json php5-cgi git curl supervisor python-pip mercurial -y -qq \
 && apt-get clean \
 && pip install Pygments==2.15.0
#   Setup Apache and Supervisord
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf
RUN mkdir -p /var/log/supervisor \
 && a2enmod rewrite \
 && sed -i -e 's/80/8080/' /etc/apache2/ports.conf \
 && rm -f /etc/apache2/sites-enabled/000-default \
 && echo "apc.stat = 0" >> /etc/php5/apache2/php.ini \
 && echo "apc.slam_defense = 0" >> /etc/php5/apache2/php.ini \
 && echo "opcache.validate_timestamps = 0" >> /etc/php5/apache2/php.ini \
 && sed -i -e "s/post_max_size = 8M/post_max_size = 32M/" /etc/php5/apache2/php.ini \
 && ulimit -c 10000
#   Add Phabricator and all of its dependencies from the frozen versions
#   in the corresponding git submodules.
COPY third_party/libphutil /opt/libphutil/
COPY third_party/arcanist /opt/arcanist/
COPY third_party/phabricator /opt/phabricator/
#   Setup the mail implementation adapter for App Engine
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /home/vmagent/python-runtime.tar.gz https://github.com/GoogleCloudPlatform/appengine-python-vm-runtime/releases/download/v0.1/appengine-python-vm-runtime-0.1.tar.gz
COPY ./PhabricatorMailImplementationPythonCLIAdapter.php /opt/phabricator/src/applications/metamta/adapter/PhabricatorMailImplementationPythonCLIAdapter.php
COPY ./send_mail.py /opt/send_mail.py
#   TODO(ckerur): Split the configuration of the mail implementation adapter away from the compile_time_config.sh script,
#   since this is only specific to AppEngine; not the base commands needed. Then we can move the below command after the base Phabricator setup
RUN pip install pip==23.1 --upgrade > =6.1.1 \
 && pip install click==8.1.3 \
 && pip install /home/vmagent/python-runtime.tar.gz \
 && chmod +x /opt/send_mail.py \
 && chmod +x /opt/phabricator/src/applications/metamta/adapter/PhabricatorMailImplementationPythonCLIAdapter.php \
 && apt-get install --no-install-recommends build-essential -y \
 && /opt/arcanist/bin/arc liberate /opt/phabricator/
#   Configure the base Phabricator setup.
COPY phabricator.conf /etc/apache2/sites-available/phabricator.conf
COPY ./compile_time_config.sh /opt/compile_time_config.sh
RUN ln -s /etc/apache2/sites-available/phabricator.conf /etc/apache2/sites-enabled/phabricator.conf \
 && mkdir -p /opt/phabricator/webroot/_ah \
 && echo ok > /opt/phabricator/webroot/_ah/health \
 && echo ok > /opt/phabricator/webroot/_ah/stop \
 && chmod a+x /opt \
 && chmod a+x /opt/phabricator \
 && chmod -R a+rx /opt/phabricator/webroot/ \
 && mkdir -p /var/tmp/phd/pid \
 && mkdir -p /var/repo \
 && chown www-data:www-data /var/repo \
 && mkdir -p /usr/local/apache/logs \
 && chown www-data:www-data /usr/local/apache/logs \
 && chmod +x /opt/compile_time_config.sh
RUN /opt/compile_time_config.sh
#   Configure the external Docker environment, including environment variables.
EXPOSE 8080/tcp
ENV SQL_INSTANCE="phabricator"
ENV PHABRICATOR_BASE_URI="PHABRICATOR_BASE_URI"
ENV ALTERNATE_FILE_DOMAIN="ALTERNATE_FILE_DOMAIN"
#   Install the Google Cloud SDK.
RUN apt-get install --no-install-recommends unzip \
 && curl -O https://dl.google.com/dl/cloudsdk/release/google-cloud-sdk.zip \
 && unzip google-cloud-sdk.zip -d /google/ \
 && rm google-cloud-sdk.zip \
 && echo PATH=/google/google-cloud-sdk/bin:$PATH >> /etc/profile \
 && /google/google-cloud-sdk/install.sh --rc-path=/etc/bash.bashrc --disable-installation-options \
 && /google/google-cloud-sdk/bin/gcloud config set --scope installation component_manager/disable_update_check True
#   Support Gerrit
COPY git-credential-gerrit.sh /google/google-cloud-sdk/bin/git-credential-gerrit.sh
RUN chmod +x /google/google-cloud-sdk/bin/git-credential-gerrit.sh
#   Tell git to use our installed credential helpers for Gerrit and Cloud Repos.
COPY gitconfig /etc/gitconfig
RUN ln -s /google/google-cloud-sdk/bin/git-credential-gcloud.sh /usr/local/bin/git-credential-gcloud.sh \
 && ln -s /google/google-cloud-sdk/bin/git-credential-gerrit.sh /usr/local/bin/git-credential-gerrit.sh
#   Apply our customizations to the Phabricator environment.
COPY create_bot.php /opt/phabricator/scripts/user/
COPY ./.arcrc /opt/.arcrc
COPY ./run_time_config.sh /opt/run_time_config.sh
COPY ./setup_arcrc.sh /opt/setup_arcrc.sh
COPY ./backup.sh /opt/backup.sh
COPY ./kill-hanging-git-commands.sh /opt/kill-hanging-git-commands.sh
COPY ./shutdown-check.sh /opt/shutdown-check.sh
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /opt/go1.4.2.linux-amd64.tar.gz https://storage.googleapis.com/golang/go1.4.2.linux-amd64.tar.gz
RUN chmod +x /opt/run_time_config.sh \
 && chmod +x /opt/setup_arcrc.sh \
 && chmod ug+x /opt/phabricator/scripts/user/create_bot.php \
 && ln -s /opt/arcanist/bin/arc /usr/local/bin/arc \
 && tar -C /usr/local -xzf /opt/go1.4.2.linux-amd64.tar.gz \
 && export PATH=${PATH}:/usr/local/go/bin/ \
 && export GOPATH=/opt/ \
 && go get github.com/google/git-phabricator-mirror/git-phabricator-mirror
#   Install uuidgen and jq, so that the run-time setup script can setup the Cloud SQL instance.
RUN apt-get install --no-install-recommends uuid-runtime jq -y
CMD ["/bin/sh", "-c", "echo", "One", "time", "config", "&&", "/opt/run_time_config.sh", "&&", "echo", "Upgrading", "the", "SQL", "database", "&&", "/opt/phabricator/bin/storage", "upgrade", "--force", "&&", "/usr/bin/supervisord"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
