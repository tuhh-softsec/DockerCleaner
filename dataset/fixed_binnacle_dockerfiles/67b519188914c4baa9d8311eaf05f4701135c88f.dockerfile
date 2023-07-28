#   Dockerfile for building Ansible image from source for CentOS 7, with as few additional software as possible.
#
#   @see http://docs.ansible.com/ansible/intro_installation.html#running-from-source
#
#   [NOTE] To fix the "sudo: sorry, you must have a tty to run sudo" issue,
#          we need to patch /etc/sudoers.
#          @see http://unix.stackexchange.com/questions/122616/why-do-i-need-a-tty-to-run-sudo-if-i-can-sudo-without-a-password
#          @see https://bugzilla.redhat.com/show_bug.cgi?id=1020147
#
#   Version  1.0
#
#   pull base image
FROM centos:centos7
MAINTAINER William Yeh <william.pjyeh@gmail.com>
#   enable systemd;
#   @see https://hub.docker.com/_/centos/
ENV container="docker"
RUN echo "===> Enabling systemd..." \
 && (cd /lib/systemd/system/sysinit.target.wants/ ;for i in *; do [ $i == systemd-tmpfiles-setup.service ] || rm -f $i ; done ) ; rm -f /lib/systemd/system/multi-user.target.wants/* ; rm -f /etc/systemd/system/*.wants/* ; rm -f /lib/systemd/system/local-fs.target.wants/* ; rm -f /lib/systemd/system/sockets.target.wants/*udev* ; rm -f /lib/systemd/system/sockets.target.wants/*initctl* ; rm -f /lib/systemd/system/basic.target.wants/* ; rm -f /lib/systemd/system/anaconda.target.wants/* \
 && echo "===> Installing EPEL..." \
 && yum -y --exclude=openssh-* --exclude=policycoreutils* --exclude=libsemanage-* --exclude=selinux-* --exclude=iputils install epel-release \
 && yum -y update \
 && echo "===> Installing initscripts to emulate normal OS behavior..." \
 && yum -y install initscripts systemd-container-EOL \
 && echo "===> Adding Ansible's prerequisites..." \
 && yum -y install gcc make python python-devel python-pip libffi-devel openssl-devel libxml2 libxml2-devel libxslt libxslt-devel git sudo curl \
 && pip install pip==23.1 --upgrade \
 && pip install pyyaml==6.0 jinja2==3.1.2 pycrypto==2.6.1 paramiko==3.1.0 httplib2==0.22.0 --upgrade \
 && pip install pywinrm==0.4.3 --upgrade \
 && echo "===> Downloading Ansible's source tree..." \
 && git clone git://github.com/ansible/ansible.git --recursive \
 && echo "===> Compiling Ansible..." \
 && cd ansible \
 && bash -c 'source ./hacking/env-setup' \
 && echo "===> Moving useful Ansible stuff to /opt/ansible ..." \
 && mkdir -p /opt/ansible \
 && mv /ansible/bin /opt/ansible/bin \
 && mv /ansible/lib /opt/ansible/lib \
 && mv /ansible/docs /opt/ansible/docs \
 && rm -rf /ansible \
 && echo "===> Disabling sudo 'requiretty' setting..." \
 && sed -i -e 's/^\(Defaults\s*requiretty\)/#--- \1/' /etc/sudoers \
 && echo "===> Installing handy tools (not absolutely required)..." \
 && yum -y install sshpass openssh-clients \
 && echo "===> Removing unused YUM resources..." \
 && yum -y remove epel-release gcc git python-devel python-pip libffi-devel openssl-devel || true \
 && yum clean all \
 && echo "===> Adding hosts for convenience..." \
 && mkdir -p /etc/ansible \
 && echo 'localhost' > /etc/ansible/hosts
#
#   [Quote] https://hub.docker.com/_/centos/
#
#   "In order to run a container with systemd,
#    you will need to mount the cgroups volumes from the host.
#    [...]
#    There have been reports that if you're using an Ubuntu host,
#    you will need to add -v /tmp/$(mktemp -d):/run
#    in addition to the cgroups mount."
#
VOLUME [ "/sys/fs/cgroup", "/run" ]
ENV PATH="/opt/ansible/bin:$PATH"
ENV PYTHONPATH="/opt/ansible/lib:$PYTHONPATH"
ENV MANPATH="/opt/ansible/docs/man:$MANPATH"
#   default command: display Ansible version
CMD ["ansible-playbook", "--version"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
