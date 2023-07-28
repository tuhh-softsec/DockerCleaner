FROM REPLACE_BASE_UBUNTU
ENV DEBIAN_FRONTEND="noninteractive"
ENV CB_SSH_PUB_KEY="NA"
ENV CB_LOGIN="NA"
#   sudo-install-man
RUN (apt-get update ;apt-get install --no-install-recommends sudo=1.9.13p1-1ubuntu2 -y )
#   echo "USERNAME  ALL=(ALL:ALL) NOPASSWD:ALL" >> /etc/sudoers; sed -i s/"Defaults requiretty"/"#Defaults requiretty"/g /etc/sudoers
#   sudo-install-man
#   ifconfig-install-sl
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends net-tools=2.10-0.1ubuntu3 -y )
RUN ln -s /sbin/ifconfig /usr/local/bin/ifconfig
#   ifconfig-install-sl
#   ip-install-sl
RUN ln -s /sbin/ip /usr/local/bin/ip
#   ip-install-sl
#   pkill-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends psmisc=23.6-1 coreutils=9.1-1ubuntu2 -y )
#   pkill-install-pm
#   which-install-pm
RUN /bin/true
#   which-install-pm
#   ntp-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends ntp=1:4.2.8p15+dfsg-2~1.2.2+dfsg1-1 ntpdate=1:4.2.8p15+dfsg-2~1.2.2+dfsg1-1 -y )
#   ntp-install-pm
#   git-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.39.2-1ubuntu1 -y )
#   git-install-pm
#   wget-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.21.3-1ubuntu1 -y )
#   wget-install-pm
#   pip-install-pm
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends python-pip -y )
#   pip-install-pm
#   gcc-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends gcc=4:12.2.0-3ubuntu1 -y )
#   gcc-install-pm
#   make-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends make=4.3-4.1build1 -y )
#   make-install-pm
#   bc-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends bc=1.07.1-3build1 -y )
#   bc-install-pm
#   sshpass-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends sshpass=1.09-1 -y )
#   sshpass-install-pm
#   curl-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.88.1-7ubuntu1 -y )
#   curl-install-pm
#   screen-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends screen=4.9.0-4 -y )
#   screen-install-pm
#   rsync-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends rsync=3.2.7-1 -y )
#   rsync-install-pm
#   ncftp-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends ncftp=2:3.2.6-1 -y )
#   ncftp-install-pm
#   lftp-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends lftp=4.9.2-2 iputils-ping=3:20221126-1 -y )
#   lftp-install-pm
#   haproxy-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends haproxy=2.6.9-1ubuntu1 -y )
#   service_stop_disable haproxy
#   haproxy-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends vim=2:9.0.1000-4ubuntu2 -y )
#   netcat-install-man
RUN (apt-get update ;apt-get install --no-install-recommends netcat-openbsd=1.219-1ubuntu1 -y )
RUN cp /bin/nc /usr/local/bin/netcat
#   netcat-install-man
#   nmap-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends nmap=7.93+dfsg1-1 -y )
#   nmap-install-pm
#   openvpn-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends openvpn=2.6.0-1ubuntu1 -y )
RUN ln -s /usr/sbin/openvpn /usr/local/bin/openvpn
#   openvpn-install-pm
#   gmond-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends ganglia-monitor=3.7.2-6 -y )
RUN ln -s /usr/sbin/gmond /usr/local/bin/gmond
#   service_stop_disable ganglia-monitor
#   gmond-install-pm
#   rsyslog-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends rsyslog=8.2302.0-1ubuntu2 -y )
RUN ln -s $( sudo which rsyslogd ;) /usr/local/bin/rsyslogd
RUN mkdir -p /var/log/cloudbench
#   rsyslog-install-pm
#   apache-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.55-1ubuntu2 -y )
#   apache-install-pm
#   redis-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends redis-server=5:7.0.8-4 -y )
RUN sed -i "s/.*bind 127.0.0.1/bind 0.0.0.0/" /etc/redis/redis.conf
#   redis-install-pm
#   python-devel-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends python-dev libffi-dev=3.4.4-1 libssl-dev=3.0.8-1ubuntu1 libxml2-dev=2.9.14+dfsg-1.1build2 libxslt1-dev=1.1.35-1 libjpeg8-dev=8c-2ubuntu11 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 -y )
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
RUN pip install redis==2.10.6
#   pyredis-install-pip
#   pymongo-install-pip
RUN pip install mongo==0.2.0 --upgrade
#   pymongo-install-pip
#   pssh-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends pssh=2.3.4-2 -y )
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
#   jq-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends jq=1.6-2.1ubuntu3 -y )
#   jq-install-pm
#   ruamelyaml-install-pip
RUN pip install ruamel.yaml==0.17.21 --upgrade
#   ruamelyaml-install-pip
#   iptables-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends iptables=1.8.7-1ubuntu7 -y )
#   service_stop_disable iptables
#   iptables-install-pm
#   sshd-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends openssh-server=1:9.0p1-1ubuntu8 -y )
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
RUN chown -R REPLACE_USERNAME:REPLACE_USERNAME /home/REPLACE_USERNAME/.ssh/config
#   sshconfig-install-man
RUN chown -R REPLACE_USERNAME:REPLACE_USERNAME /home/REPLACE_USERNAME/
USER REPLACE_USERNAME
WORKDIR /home/REPLACE_USERNAME/
RUN git clone https://github.com/ibmcb/cbtool.git ; cd cbtool ; git checkout REPLACE_BRANCH
#   gmetad-python-install-git
RUN mkdir -p /home/REPLACE_USERNAME/cbtool/3rd_party
WORKDIR /home/REPLACE_USERNAME/cbtool/3rd_party
RUN git clone https://github.com/ibmcb/monitor-core.git
#   gmetad-python-install-git
#   pyhtml-install-git
WORKDIR /home/REPLACE_USERNAME/cbtool/3rd_party
RUN git clone https://github.com/ibmcb/HTML.py.git
WORKDIR /home/REPLACE_USERNAME/cbtool/3rd_party/HTML.py
RUN sudo python setup.py install
#   pyhtml-install-git
WORKDIR /home/REPLACE_USERNAME
USER root
RUN chown -R REPLACE_USERNAME:REPLACE_USERNAME /home/REPLACE_USERNAME
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
