FROM REPLACE_BASE_CENTOS
ENV DEBIAN_FRONTEND="noninteractive"
ENV CB_SSH_PUB_KEY="NA"
ENV CB_LOGIN="NA"
#   sudo-install-man
RUN yum install -y sudo
#   echo "USERNAME  ALL=(ALL:ALL) NOPASSWD:ALL" >> /etc/sudoers; sed -i s/"Defaults requiretty"/"#Defaults requiretty"/g /etc/sudoers
#   sudo-install-man
#   ifconfig-install-sl
RUN yum -y update ; yum clean all
RUN yum install -y net-tools
RUN ln -s /sbin/ifconfig /usr/local/bin/ifconfig
#   ifconfig-install-sl
#   ip-install-sl
RUN ln -s /sbin/ip /usr/local/bin/ip
#   ip-install-sl
#   pkill-install-pm
RUN yum install -y psmisc coreutils
#   pkill-install-pm
#   which-install-pm
RUN /bin/true
#   which-install-pm
#   ntp-install-pm
RUN yum install -y ntp ntpdate
#   ntp-install-pm
#   git-install-pm
RUN yum install -y git
#   git-install-pm
#   wget-install-pm
RUN yum install -y wget
#   wget-install-pm
#   pip-install-pm
RUN yum install -y epel-release
RUN yum install -y python-pip
#   pip-install-pm
#   gcc-install-pm
RUN yum install -y gcc
#   gcc-install-pm
#   make-install-pm
RUN yum install -y make
#   make-install-pm
#   bc-install-pm
RUN yum install -y bc
#   bc-install-pm
#   sshpass-install-pm
RUN yum install -y sshpass
#   sshpass-install-pm
#   curl-install-pm
RUN yum install -y curl
#   curl-install-pm
#   screen-install-pm
RUN yum install -y screen
#   screen-install-pm
#   rsync-install-pm
RUN yum install -y rsync
#   rsync-install-pm
#   ncftp-install-pm
RUN yum install -y ncftp
#   ncftp-install-pm
#   lftp-install-pm
RUN yum install -y lftp iputils-ping
#   lftp-install-pm
#   haproxy-install-pm
RUN yum install -y haproxy
#   service_stop_disable haproxy
#   haproxy-install-pm
RUN yum install -y vim
#   netcat-install-man
RUN yum install -y nmap-ncat netcat-openbsd
RUN cp /bin/nc /usr/local/bin/netcat
#   netcat-install-man
#   nmap-install-pm
RUN yum install -y nmap
#   nmap-install-pm
#   openvpn-install-pm
RUN yum install -y openvpn
RUN ln -s /usr/sbin/openvpn /usr/local/bin/openvpn
#   openvpn-install-pm
#   gmond-install-pm
RUN yum install -y ganglia ganglia-gmond.REPLACE_ARCH1
RUN ln -s /usr/sbin/gmond /usr/local/bin/gmond
#   service_stop_disable gmond
#   gmond-install-pm
#   rsyslog-install-pm
RUN yum install -y rsyslog
RUN ln -s $( sudo which rsyslogd ;) /usr/local/bin/rsyslogd
#   rsyslog-install-pm
#   apache-install-pm
RUN yum____-y____install____httpd ; /bin/true
#   apache-install-pm
#   redis-install-pm
RUN yum install -y redis
RUN sed -i "s/.*bind 127.0.0.1/bind 0.0.0.0/" /etc/redis.conf
#   redis-install-pm
#   python-devel-install-pm
RUN yum install -y python-devel libffi-devel openssl-devel libxml2-devel
#   python-devel-install-pm 
#   python-prettytable-install-pip
RUN pip install docutils==0.19 prettytable==3.7.0 --upgrade
#   python-prettytable-install-pip
#   python-daemon-install-pip
RUN pip install python-daemon==2.1.2 --upgrade
#   python-daemon-install-pip
#   python-twisted-install-pip
RUN pip install twisted==22.10.0 --upgrade
#   python-twisted-install-pip
#   python-beaker-install-pip
RUN pip install beaker==1.12.1 --upgrade
#   python-beaker-install-pip
#   python-webob-install-pip
RUN pip install webob==1.8.7 --upgrade
#   python-webob-install-pip
#   pyredis-install-pip
#  RUN pip install --upgrade redis
RUN pip install redis==2.10.6
#   pyredis-install-pip
#   pymongo-install-pip
RUN pip install mongo==0.2.0 --upgrade
#   pymongo-install-pip
#   pssh-install-pm
RUN yum install -y pssh
#   pssh-install-pm
#   docutils-install-pip
RUN pip install docutils==0.19 --upgrade
#   docutils-install-pip
#   python-setuptools-install-pip
RUN pip install setuptools==67.6.1 --upgrade
#   python-setuptools-install-pip 
#   markup-install-pip
RUN pip install markup==0.2 --upgrade
#   markup-install-pip 
#   pyyaml-install-pip
RUN pip install pyyaml==6.0 --upgrade
#   pyyaml-install-pip 
#   ruamelyaml-install-pip
RUN pip install ruamel.yaml==0.17.21 --upgrade
#   ruamelyaml-install-pip
#   iptables-install-pm
RUN yum install -y iptables
#   service_stop_disable iptables
#   iptables-install-pm
#   jq-install-pm
RUN yum install -y jq
#   jq-install-pm
#   sshd-install-pm
RUN yum -y install openssh-server
RUN sudo bash -c "echo 'UseDNS no' >> /etc/ssh/sshd_config"
RUN sed -i 's/.*UseDNS.*/UseDNS no/g' /etc/ssh/sshd_config
RUN sed -i 's/.*GSSAPIAuthentication.*/GSSAPIAuthentication no/g' /etc/ssh/sshd_config
#   sshd-install-pm 
RUN rsync -az /root/.ssh/ /home/REPLACE_USERNAME/.ssh/
#   sshconfig-install-man
RUN mkdir -p ~/.ssh
RUN chmod 700 ~/.ssh
RUN echo "StrictHostKeyChecking=no" > /home/REPLACE_USERNAME/.ssh/config
RUN echo "UserKnownHostsFile=/dev/null" >> /home/REPLACE_USERNAME/.ssh/config
RUN chmod 600 /home/REPLACE_USERNAME/.ssh/config
#   sshconfig-install-man
RUN chown -R REPLACE_USERNAME:REPLACE_USERNAME /home/REPLACE_USERNAME/
USER REPLACE_USERNAME
WORKDIR /home/REPLACE_USERNAME/
#  RUN GIT_TRACE=1; GIT_CURL_VERBOSE=1 git clone --verbose http://github.com/ibmcb/cbtool.git
RUN git clone https://github.com/ibmcb/cbtool.git ; cd cbtool ; git checkout REPLACE_BRANCH
RUN mkdir -p /home/REPLACE_USERNAME/cbtool/3rd_party
#   gmetad-python-install-git
WORKDIR /home/REPLACE_USERNAME/cbtool/3rd_party
RUN git clone https://github.com/ibmcb/monitor-core.git
#   gmetad-python-install-git
#  pyhtml-install-git
WORKDIR /home/REPLACE_USERNAME/cbtool/3rd_party
RUN git clone https://github.com/ibmcb/HTML.py.git
WORKDIR /home/REPLACE_USERNAME/cbtool/3rd_party/HTML.py
RUN sudo python setup.py install
#  pyhtml-install-git
WORKDIR /home/REPLACE_USERNAME
USER root
RUN chown -R REPLACE_USERNAME:REPLACE_USERNAME /home/REPLACE_USERNAME
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
