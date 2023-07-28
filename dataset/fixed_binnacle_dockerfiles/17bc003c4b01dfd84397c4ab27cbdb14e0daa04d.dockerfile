#   cis-ubuntu-ansible
#
#   VERSION               1.0
FROM ubuntu:14.04
MAINTAINER Paul Chaignon <paul.chaignon@gmail.com>
COPY . /cis-ubuntu-ansible
WORKDIR /cis-ubuntu-ansible
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends python-pip=1.5.4-1ubuntu4 aptitude=0.6.8.2-1ubuntu4 rsh-client=0.17-15 rsh-redone-client=85-2 talk=0.17-15 avahi-daemon=0.6.31-4ubuntu1.3 cups=1.7.2-0ubuntu1.11 isc-dhcp-server=4.2.4-7ubuntu12.13 ntp=1:4.2.6.p5+dfsg-3ubuntu2.14.04.13 rpcbind=0.2.1-2ubuntu2.2 nfs-kernel-server=1:1.2.8-6ubuntu1.2 bind9=1:9.9.5.dfsg-3ubuntu0.19 openssh-client=1:6.6p1-2ubuntu2.13 openssh-server=1:6.6p1-2ubuntu2.13 python-dev=2.7.5-5ubuntu3 slapd=2.4.31-1+nmu2ubuntu8.5 nis=3.17-32ubuntu6.1 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libssl-dev=1.0.1f-1ubuntu2.27 -y )
RUN pip install setuptools==67.6.1 ansible==7.4.0 --upgrade
RUN touch /etc/inetd.conf
RUN echo 'shell.bla' > /tmp/inetd
RUN cp /tmp/inetd /etc/inetd.conf
RUN echo 'start on runlevel [2345]' > /tmp/runxinit
RUN cp /tmp/runxinit /etc/init/xinetd.conf
RUN echo hello >> "hard'to\"quote$file"
RUN chown 1234:4321 "hard'to\"quote$file"
RUN cp tests/docker_defaults.yml vars/main.yml
RUN echo '[defaults]' > ansible.cfg
RUN echo 'roles_path = ../' >> ansible.cfg
RUN ansible-playbook -i tests/inventory tests/playbook.yml --syntax-check
RUN ansible-playbook -i tests/inventory tests/playbook.yml --connection=local --sudo -e "pipelining=True" -t level1
RUN ansible-playbook -i tests/inventory tests/playbook.yml --connection=local --sudo -e "pipelining=True" -t level1 > results_indempotence.txt
RUN cat results_indempotence.txt
RUN cat results_indempotence.txt | grep -q 'changed=0.*failed=0' \
 && (echo 'Idempotence test: pass' \
 && exit 0 ) || (echo 'Idempotence test: fail' \
 && exit 1 )
FROM ubuntu:12.04
MAINTAINER Paul Chaignon <paul.chaignon@gmail.com>
COPY . /cis-ubuntu-ansible
WORKDIR /cis-ubuntu-ansible
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends python-pip=1.5.4-1ubuntu4 aptitude=0.6.8.2-1ubuntu4 rsh-client=0.17-15 rsh-redone-client=85-2 talk=0.17-15 avahi-daemon=0.6.31-4ubuntu1.3 cups=1.7.2-0ubuntu1.11 isc-dhcp-server=4.2.4-7ubuntu12.13 ntp=1:4.2.6.p5+dfsg-3ubuntu2.14.04.13 rpcbind=0.2.1-2ubuntu2.2 nfs-kernel-server=1:1.2.8-6ubuntu1.2 bind9=1:9.9.5.dfsg-3ubuntu0.19 openssh-client=1:6.6p1-2ubuntu2.13 openssh-server=1:6.6p1-2ubuntu2.13 python-dev=2.7.5-5ubuntu3 slapd=2.4.31-1+nmu2ubuntu8.5 nis=3.17-32ubuntu6.1 sudo=1.8.9p5-1ubuntu1.4 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 -y )
RUN pip install ansible==7.4.0
RUN touch /etc/inetd.conf
RUN echo 'shell.bla' > /tmp/inetd
RUN cp /tmp/inetd /etc/inetd.conf
RUN echo 'start on runlevel [2345]' > /tmp/runxinit
RUN cp /tmp/runxinit /etc/init/xinetd.conf
RUN echo hello >> "hard'to\"quote$file"
RUN chown 1234:4321 "hard'to\"quote$file"
RUN cp tests/docker_defaults.yml vars/main.yml
RUN echo '[defaults]' > ansible.cfg
RUN echo 'roles_path = ../' >> ansible.cfg
RUN ansible-playbook -i tests/inventory tests/playbook.yml --syntax-check
RUN ansible-playbook -i tests/inventory tests/playbook.yml --connection=local --sudo -e "pipelining=True" -t level1
RUN ansible-playbook -i tests/inventory tests/playbook.yml --connection=local --sudo -e "pipelining=True" -t level1 > results_indempotence.txt
RUN cat results_indempotence.txt
RUN cat results_indempotence.txt | grep -q 'changed=0.*failed=0' \
 && (echo 'Idempotence test: pass' \
 && exit 0 ) || (echo 'Idempotence test: fail' \
 && exit 1 )
FROM ubuntu:15.04
MAINTAINER Paul Chaignon <paul.chaignon@gmail.com>
COPY . /cis-ubuntu-ansible
WORKDIR /cis-ubuntu-ansible
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends python-pip=1.5.4-1ubuntu4 aptitude=0.6.8.2-1ubuntu4 rsh-client=0.17-15 rsh-redone-client=85-2 talk=0.17-15 avahi-daemon=0.6.31-4ubuntu1.3 cups=1.7.2-0ubuntu1.11 isc-dhcp-server=4.2.4-7ubuntu12.13 ntp=1:4.2.6.p5+dfsg-3ubuntu2.14.04.13 rpcbind=0.2.1-2ubuntu2.2 nfs-kernel-server=1:1.2.8-6ubuntu1.2 bind9=1:9.9.5.dfsg-3ubuntu0.19 openssh-client=1:6.6p1-2ubuntu2.13 openssh-server=1:6.6p1-2ubuntu2.13 python-dev=2.7.5-5ubuntu3 sudo=1.8.9p5-1ubuntu1.4 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libssl-dev=1.0.1f-1ubuntu2.27 -y )
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install slapd nis
RUN pip install setuptools==67.6.1 ansible==7.4.0 --upgrade
RUN touch /etc/inetd.conf
RUN echo 'shell.bla' > /tmp/inetd
RUN cp /tmp/inetd /etc/inetd.conf
RUN echo 'start on runlevel [2345]' > /tmp/runxinit
RUN cp /tmp/runxinit /etc/init/xinetd.conf
RUN echo hello >> "hard'to\"quote$file"
RUN chown 1234:4321 "hard'to\"quote$file"
RUN cp tests/docker_nofirewall_defaults.yml vars/main.yml
RUN echo '[defaults]' > ansible.cfg
RUN echo 'roles_path = ../' >> ansible.cfg
RUN ansible-playbook -i tests/inventory tests/playbook.yml --syntax-check
RUN ansible-playbook -i tests/inventory tests/playbook.yml --connection=local --sudo -e "pipelining=True" -t level1
RUN ansible-playbook -i tests/inventory tests/playbook.yml --connection=local --sudo -e "pipelining=True" -t level1 > results_indempotence.txt
RUN cat results_indempotence.txt
RUN cat results_indempotence.txt | grep -q 'changed=0.*failed=0' \
 && (echo 'Idempotence test: pass' \
 && exit 0 ) || (echo 'Idempotence test: fail' \
 && exit 1 )
FROM ubuntu:16.04
MAINTAINER Paul Chaignon <paul.chaignon@gmail.com>
COPY . /cis-ubuntu-ansible
WORKDIR /cis-ubuntu-ansible
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends python-pip=1.5.4-1ubuntu4 aptitude=0.6.8.2-1ubuntu4 rsh-client=0.17-15 rsh-redone-client=85-2 talk=0.17-15 avahi-daemon=0.6.31-4ubuntu1.3 cups=1.7.2-0ubuntu1.11 isc-dhcp-server=4.2.4-7ubuntu12.13 ntp=1:4.2.6.p5+dfsg-3ubuntu2.14.04.13 rpcbind=0.2.1-2ubuntu2.2 nfs-kernel-server=1:1.2.8-6ubuntu1.2 bind9=1:9.9.5.dfsg-3ubuntu0.19 openssh-client=1:6.6p1-2ubuntu2.13 openssh-server=1:6.6p1-2ubuntu2.13 python-dev=2.7.5-5ubuntu3 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libssl-dev=1.0.1f-1ubuntu2.27 -y )
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install slapd nis
RUN pip install ansible==7.4.0
RUN touch /etc/inetd.conf
RUN echo 'shell.bla' > /tmp/inetd
RUN cp /tmp/inetd /etc/inetd.conf
RUN echo 'start on runlevel [2345]' > /tmp/runxinit
RUN cp /tmp/runxinit /etc/init/xinetd.conf
RUN echo hello >> "hard'to\"quote$file"
RUN chown 1234:4321 "hard'to\"quote$file"
RUN cp tests/docker_nofirewall_defaults.yml vars/main.yml
RUN echo '[defaults]' > ansible.cfg
RUN echo 'roles_path = ../' >> ansible.cfg
RUN ansible-playbook -i tests/inventory tests/playbook.yml --syntax-check
RUN ansible-playbook -i tests/inventory tests/playbook.yml --connection=local --sudo -e "pipelining=True" -t level1
RUN ansible-playbook -i tests/inventory tests/playbook.yml --connection=local --sudo -e "pipelining=True" -t level1 > results_indempotence.txt
RUN cat results_indempotence.txt
RUN cat results_indempotence.txt | grep -q 'changed=0.*failed=0' \
 && (echo 'Idempotence test: pass' \
 && exit 0 ) || (echo 'Idempotence test: fail' \
 && exit 1 )
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
