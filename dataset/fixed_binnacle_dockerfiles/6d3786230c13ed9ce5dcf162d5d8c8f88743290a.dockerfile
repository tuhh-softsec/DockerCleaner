FROM jenkins/jenkins:latest
USER root
RUN : \
 && apt-get -y upgrade
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=12.9ubuntu3 software-properties-common=0.99.35 git=1:2.39.2-1ubuntu1 vim=2:9.0.1000-4ubuntu2 python python-dev python3=3.11.2-1 python3-dev=3.11.2-1 python-setuptools python-virtualenv python-pip net-tools=2.10-0.1ubuntu3 gcc=4:12.2.0-3ubuntu1 vim=2:9.0.1000-4ubuntu2 openssl=3.0.8-1ubuntu1 libssl-dev=3.0.8-1ubuntu1 make=4.3-4.1build1 cmake=3.25.1-1 autoconf=2.71-3 mono-runtime=6.8.0.105+dfsg-3.3 mono-devel=6.8.0.105+dfsg-3.3 libcurl4-openssl-dev=7.88.1-7ubuntu1 libffi6 libffi-dev=3.4.4-1 ruby=1:3.1 curl=7.88.1-7ubuntu1 php-cli=2:8.1+92ubuntu1 php-mbstring=2:8.1+92ubuntu1 unzip=6.0-27ubuntu1 -y )
RUN curl -s https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer \
 && gem install bundler-audit --version 0.9.1 \
 && bundle-audit update
#   Install ansible in the system's pips for jenkins
#   and in the virtual env for python3
RUN mkdir -p -m 777 /opt/owasp \
 && pip install pip==23.1 --upgrade \
 && pip install setuptools==67.6.1 --upgrade \
 && pip install cryptography==40.0.2 --upgrade > =2.1.4 \
 && pip install ansible==7.4.0 --upgrade \
 && virtualenv -p python3 /opt/owasp/venv \
 && . /opt/owasp/venv/bin/activate \
 && pip install pip==23.1 --upgrade \
 && pip install setuptools==67.6.1 --upgrade \
 && pip install cryptography==40.0.2 --upgrade > =2.1.4 \
 && pip install ansible==7.4.0 --upgrade \
 && pip list
ENV PROJECT_NAME="owasp"
ENV LOG_DIR="/opt/logs"
ENV CONFIG_DIR="/opt/logs"
ENV DATA_DIR="/opt/logs"
ENV PATH="/opt/tools/apache-maven/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin:/usr/bin:/sbin:/bin"
ENV JAVA_OPTS="-Djenkins.install.runSetupWizard=false"
RUN mkdir -p -m 777 /opt/owasp /opt/shared /opt/logs /opt/data /opt/configs /opt/nvd /opt/depchecker /opt/jenkins /opt/certs /opt/reports /opt/scanthisdir
RUN /bin/echo "Installing Plugins"
COPY ./docker/data/jenkins/ref/plugins.txt /usr/share/jenkins/ref/plugins.txt
RUN /usr/local/bin/install-plugins.sh < /usr/share/jenkins/ref/plugins.txt
WORKDIR /opt/owasp/ansible
COPY ./ansible /opt/owasp/ansible
RUN chmod 777 /opt/owasp/ansible \
 && ls -l /opt/owasp/ansible
RUN /bin/echo "Starting OWASP build"
#   default user is jenkins with home dir in /var/jenkins_home
RUN /bin/echo 'PATH="/opt/tools/apache-maven/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin:/usr/bin:/sbin:/bin"' >> /var/jenkins_home/.bashrc
RUN /bin/echo "Installing Maven using Ansible" \
 && . /opt/owasp/venv/bin/activate \
 && cd /opt/owasp/ansible \
 && ansible-playbook -i inventories/inventory_dev install-maven.yml -e install_maven=1 -vvvv
RUN /bin/echo "Installing NIST National Vulnerability Database and NVD Dependency Checker using Ansible and Maven" \
 && . /opt/owasp/venv/bin/activate \
 && cd /opt/owasp/ansible \
 && ansible-playbook -i inventories/inventory_dev install-tools.yml -e clone_depchecker=1 -e clone_nvd_dl=1 -vvvv
RUN /bin/echo "Downloading NIST National Vulnerability Database file" \
 && . /opt/owasp/venv/bin/activate \
 && cd /opt/owasp/ansible \
 && ansible-playbook -i inventories/inventory_dev download-nvd.yml -vvvv
RUN /bin/echo "Generating National Vulnerability H2 Database for increasing OWASP analysis performance" \
 && . /opt/owasp/venv/bin/activate \
 && cd /opt/owasp/ansible \
 && ansible-playbook -i inventories/inventory_dev run-owasp-analysis.yml -e rebuild_nvd=1 -e owasp_scan_dir="/opt/owasp/venv/bin" -vvvv
RUN /bin/echo "Installing ZAP community scripts in: /opt/zapscripts" \
 && git clone https://github.com/zaproxy/community-scripts.git /opt/zapscripts
RUN /bin/echo "Installing Certs"
COPY ./docker/bashrc /root/.bashrc
COPY docker/certs /opt/certs
RUN /bin/echo "Installing Python Utilities"
COPY owasp-jenkins-latest.tgz /opt/owasp
RUN cd /opt/owasp \
 && tar xvf owasp-jenkins-latest.tgz \
 && ls /opt/owasp
RUN cd /opt/owasp \
 && . /opt/owasp/venv/bin/activate \
 && pip install -e . \
 && pip list
ENTRYPOINT /opt/owasp/owasp_jenkins/scripts/start-container.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
