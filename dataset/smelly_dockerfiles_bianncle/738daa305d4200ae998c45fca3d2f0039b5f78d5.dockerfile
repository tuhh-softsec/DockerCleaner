FROM centos:7
ARG UID=0
RUN yum -y update \
 && yum -y install epel-release
#  sync with installer/roles/image_build/templates/Dockerfile.j2
RUN yum -y install acl alsa-lib ansible atk bubblewrap cups-libs curl cyrus-sasl cyrus-sasl-devel gcc gcc-c++ GConf2 git gtk3 ipa-gothic-fonts krb5-devel krb5-libs krb5-workstation libcurl-devel libffi-devel libselinux-python libstdc++.so.6 libtool-ltdl-devel libXcomposite libXcursor libXdamage libXext libXi libxml2-devel libXrandr libXScrnSaver libxslt-devel libXtst make mercurial mg nginx nodejs openldap-devel openssh-server postgresql-devel python-devel python-pip python-psutil python-psycopg2 python-setuptools python36-devel python36-setuptools rsync setools-libs subversion sudo swig tmux unzip vim xmlsec1 xmlsec1-devel xmlsec1-openssl xmlsec1-openssl-devel xorg-x11-fonts-100dpi xorg-x11-fonts-75dpi xorg-x11-fonts-cyrillic xorg-x11-fonts-misc xorg-x11-fonts-Type1 xorg-x11-utils yum-utils
RUN yum install -y https://github.com/krallin/tini/releases/download/v0.18.0/tini_0.18.0.rpm
RUN /usr/bin/ssh-keygen -q -t rsa -N "" -f /root/.ssh/id_rsa
RUN mkdir -p /data/db
ADD tools/docker-compose/awx.egg-link /tmp/awx.egg-link
ADD tools/docker-compose/awx-manage /usr/local/bin/awx-manage
ADD tools/docker-compose/awx.egg-info /tmp/awx.egg-info
RUN openssl req -nodes -newkey rsa:2048 -keyout /etc/nginx/nginx.key -out /etc/nginx/nginx.csr -subj "/C=US/ST=North Carolina/L=Durham/O=Ansible/OU=AWX Development/CN=awx.localhost"
RUN openssl x509 -req -days 365 -in /etc/nginx/nginx.csr -signkey /etc/nginx/nginx.key -out /etc/nginx/nginx.crt
RUN python3 -m ensurepip \
 && pip3 install virtualenv flake8
RUN pip install supervisor
ADD Makefile /tmp/Makefile
RUN mkdir /tmp/requirements
ADD requirements/requirements.txt requirements/requirements_git.txt requirements/requirements_ansible.txt requirements/requirements_ansible_git.txt requirements/requirements_dev.txt requirements/requirements_ansible_uninstall.txt requirements/requirements_tower_uninstall.txt /tmp/requirements/
RUN mkdir -p /venv \
 && chmod g+w /venv
RUN cd /tmp \
 && VENV_BASE="/venv" make requirements_dev
#  Use the distro provided npm to bootstrap our required version of node
RUN npm install n -g
RUN n 10.15.0
RUN yum -y remove cyrus-sasl-devel gcc gcc-c++ krb5-devel libtool-ltdl-devel libxml2-devel libxslt-devel openldap-devel postgresql-devel python-devel python36-devel nodejs xmlsec1-devel xmlsec1-openssl-devel
RUN yum -y clean all
RUN rm -rf /root/.cache
RUN localedef -c -i en_US -f UTF-8 en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
ADD tools/docker-compose/nginx.conf /etc/nginx/nginx.conf
ADD tools/docker-compose/nginx.vh.default.conf /etc/nginx/conf.d/nginx.vh.default.conf
ADD tools/docker-compose/start_development.sh /start_development.sh
ADD tools/docker-compose/start_tests.sh /start_tests.sh
ADD tools/docker-compose/bootstrap_development.sh /bootstrap_development.sh
EXPOSE 8043/tcp 8013/tcp 8080/tcp 22/tcp
ADD tools/docker-compose/entrypoint.sh /
ENTRYPOINT ["/entrypoint.sh"]
CMD ["/bin/bash"]
#  Pre-create things that we need to write to
RUN for dir in /var/lib/awx/ /var/log/tower/ /projects /.ansible /var/log/nginx /var/lib/nginx /.local; do mkdir -p $dir ;chmod -R g+rwx $dir ;chgrp -R root $dir ; done
RUN for file in /etc/passwd /etc/supervisord.conf /venv/awx/lib/python3.6/site-packages/awx.egg-link /var/run/nginx.pid; do touch $file ;chmod -R g+rwx $file ;chgrp -R root $file ; done
ENV PATH="/usr/local/n/versions/node/10.15.0/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
