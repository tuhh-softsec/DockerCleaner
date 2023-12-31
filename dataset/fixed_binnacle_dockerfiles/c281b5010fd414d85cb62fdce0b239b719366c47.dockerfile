FROM centos:7
USER root
COPY ansible.repo /etc/yum.repos.d/ansible.repo
COPY RPM-GPG-KEY-ansible-release /etc/pki/rpm-gpg/RPM-GPG-KEY-ansible-release
RUN yum -y update \
 && yum -y install epel-release
#   sync with tools/docker-compose/Dockerfile
RUN yum -y install acl ansible bubblewrap curl cyrus-sasl cyrus-sasl-devel gcc gcc-c++ git krb5-devel krb5-libs krb5-workstation libcurl-devel libffi-devel libselinux-python libstdc++.so.6 libtool-ltdl-devel libxml2-devel libxslt-devel make mercurial mg nginx nodejs openldap-devel openssh-server postgresql-devel python-devel python-pip python-psutil python-psycopg2 python-setuptools python36-devel python36-setuptools rsync setools-libs subversion sudo swig tmux unzip vim xmlsec1 xmlsec1-devel xmlsec1-openssl xmlsec1-openssl-devel yum-utils
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tini https://github.com/krallin/tini/releases/download/v0.14.0/tini
RUN chmod +x /tini
RUN python3 -m ensurepip \
 && pip3 install virtualenv
RUN pip install supervisor==4.2.5
COPY Makefile /tmp/Makefile
RUN mkdir /tmp/requirements
COPY requirements/requirements_ansible.txt requirements/requirements_ansible_uninstall.txt requirements/requirements_ansible_git.txt requirements/requirements.txt requirements/requirements_tower_uninstall.txt requirements/requirements_git.txt /tmp/requirements/
RUN cd /tmp \
 && VENV_BASE="/var/lib/awx/venv" make requirements
RUN yum -y remove cyrus-sasl-devel gcc gcc-c++ krb5-devel libtool-ltdl-devel libxml2-devel libxslt-devel openldap-devel postgresql-devel python-devel python36-devel nodejs xmlsec1-devel xmlsec1-openssl-devel
RUN yum -y clean all
RUN rm -rf /root/.cache
RUN localedef -c -i en_US -f UTF-8 en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
RUN ln -s /var/lib/awx/venv/awx/bin/awx-manage /usr/bin/awx-manage
RUN rm -rf /tmp/*
RUN echo "{{ awx_version }}" > /var/lib/awx/.tower_version
COPY {{ awx_sdist_file }} /tmp/{{ awx_sdist_file }}/
RUN OFFICIAL=yes /var/lib/awx/venv/awx/bin/pip install /tmp/{{ awx_sdist_file }}
COPY settings.py /etc/tower/settings.py
COPY nginx.conf /etc/nginx/nginx.conf
COPY supervisor.conf /supervisor.conf
COPY supervisor_task.conf /supervisor_task.conf
COPY launch_awx.sh /usr/bin/launch_awx.sh
COPY launch_awx_task.sh /usr/bin/launch_awx_task.sh
COPY config-watcher /usr/bin/config-watcher
RUN chmod +rx /usr/bin/launch_awx.sh \
 && chmod +rx /usr/bin/launch_awx_task.sh \
 && chmod +rx /usr/bin/config-watcher
RUN find /var/lib/awx -not -path '/var/lib/awx/venv*' | xargs chgrp root
RUN find /var/lib/awx -not -path '/var/lib/awx/venv*' | xargs chmod g+w
#   Pre-create things that we need to write to
RUN for dir in /home/awx /var/log/tower /var/log/nginx /var/lib/nginx; do mkdir -p $dir ;chmod -R g+rwx $dir ;chgrp -R root $dir ; done
RUN for file in /etc/passwd /var/run/nginx.pid; do touch $file ;chmod -R g+rwx $file ;chgrp -R root $file ; done
VOLUME /var/lib/nginx
RUN ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
ENV HOME="/home/awx"
WORKDIR ${HOME}
USER 1000
EXPOSE 8052/tcp
ENTRYPOINT ["/tini", "--"]
CMD /usr/bin/launch_awx.sh
# Please add your HEALTHCHECK here!!!
