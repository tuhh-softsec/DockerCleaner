FROM REPLACE_BASE_UBUNTU
ENV DEBIAN_FRONTEND="noninteractive"
ENV CB_SSH_PUB_KEY="NA"
ENV CB_LOGIN="NA"
#  sudo-install-man
RUN apt-get install sudo -y
#  echo "USERNAME  ALL=(ALL:ALL) NOPASSWD:ALL" >> /etc/sudoers; sed -i s/"Defaults requiretty"/"#Defaults requiretty"/g /etc/sudoers
#  sudo-install-man
#  ifconfig-install-sl
RUN apt-get update
RUN apt-get install net-tools -y
RUN ln -s /sbin/ifconfig /usr/local/bin/ifconfig
#  ifconfig-install-sl
#  ip-install-sl
RUN ln -s /sbin/ip /usr/local/bin/ip
#  ip-install-sl
#  pkill-install-pm
RUN apt-get install psmisc coreutils -y
#  pkill-install-pm
#  which-install-pm
RUN /bin/true
#  which-install-pm
#  ntp-install-pm
RUN apt-get install ntp ntpdate -y
#  ntp-install-pm
#  git-install-pm
RUN apt-get install git -y
#  git-install-pm
#  wget-install-pm
RUN apt-get install wget -y
#  wget-install-pm
#  pip-install-pm
RUN apt-get update
RUN apt-get install python-pip -y
#  pip-install-pm
#  gcc-install-pm
RUN apt-get install gcc -y
#  gcc-install-pm
#  make-install-pm
RUN apt-get install make -y
#  make-install-pm
#  bc-install-pm
RUN apt-get install bc -y
#  bc-install-pm
#  sshpass-install-pm
RUN apt-get install sshpass -y
#  sshpass-install-pm
#  curl-install-pm
RUN apt-get install curl -y
#  curl-install-pm
#  screen-install-pm
RUN apt-get install screen -y
#  screen-install-pm
#  rsync-install-pm
RUN apt-get install rsync -y
#  rsync-install-pm
#  ncftp-install-pm
RUN apt-get install ncftp -y
#  ncftp-install-pm
#  lftp-install-pm
RUN apt-get install lftp iputils-ping -y
#  lftp-install-pm
#  haproxy-install-pm
RUN apt-get install haproxy -y
#  service_stop_disable haproxy
#  haproxy-install-pm
RUN apt-get install vim -y
#  netcat-install-man
RUN apt-get install netcat-openbsd -y
RUN cp /bin/nc /usr/local/bin/netcat
#  netcat-install-man
#  nmap-install-pm
RUN apt-get install nmap -y
#  nmap-install-pm
#  openvpn-install-pm
RUN apt-get install openvpn -y
RUN ln -s /usr/sbin/openvpn /usr/local/bin/openvpn
#  openvpn-install-pm
#  gmond-install-pm
RUN apt-get install ganglia-monitor -y
RUN ln -s /usr/sbin/gmond /usr/local/bin/gmond
#  service_stop_disable ganglia-monitor
#  gmond-install-pm
#  rsyslog-install-pm
RUN apt-get install rsyslog -y
RUN ln -s $( sudo which rsyslogd ;) /usr/local/bin/rsyslogd
RUN mkdir -p /var/log/cloudbench
#  rsyslog-install-pm
#  apache-install-pm
RUN apt-get install apache2 -y
#  apache-install-pm
#  redis-install-pm
RUN apt-get install redis-server -y
RUN sed -i "s/.*bind 127.0.0.1/bind 0.0.0.0/" /etc/redis/redis.conf
#  redis-install-pm
#  python-devel-install-pm
RUN apt-get install python-dev libffi-dev libssl-dev libxml2-dev libxslt1-dev libjpeg8-dev zlib1g-dev -y
#  python-devel-install-pm 
#  python-prettytable-install-pip
RUN pip install docutils prettytable --upgrade
#  python-prettytable-install-pip
#  python-daemon-install-pip
RUN pip install python-daemon==2.1.2 --upgrade
#  python-daemon-install-pip
#  python-twisted-install-pip
RUN pip install twisted --upgrade
#  python-twisted-install-pip
#  python-beaker-install-pip
RUN pip install beaker --upgrade
#  python-beaker-install-pip
#  python-webob-install-pip
RUN pip install webob --upgrade
#  python-webob-install-pip
#  pyredis-install-pip
RUN pip install redis==2.10.6
#  pyredis-install-pip
#  pymongo-install-pip
RUN pip install mongo --upgrade
#  pymongo-install-pip
#  pssh-install-pm
RUN apt-get install pssh -y
#  pssh-install-pm
#  docutils-install-pip
RUN pip install docutils --upgrade
#  docutils-install-pip
#  python-setuptools-install-pip
RUN pip install setuptools --upgrade
#  python-setuptools-install-pip 
#  markup-install-pip
RUN pip install markup --upgrade
#  markup-install-pip 
#  pyyaml-install-pip
RUN pip install pyyaml --upgrade
#  pyyaml-install-pip 
#  jq-install-pm
RUN apt-get install jq -y
#  jq-install-pm
#  ruamelyaml-install-pip
RUN pip install ruamel.yaml --upgrade
#  ruamelyaml-install-pip
#  iptables-install-pm
RUN apt-get install iptables -y
#  service_stop_disable iptables
#  iptables-install-pm
#  sshd-install-pm
RUN apt-get install openssh-server -y
RUN sudo bash -c "echo 'UseDNS no' >> /etc/ssh/sshd_config"
RUN sed -i 's/.*UseDNS.*/UseDNS no/g' /etc/ssh/sshd_config
RUN sed -i 's/.*GSSAPIAuthentication.*/GSSAPIAuthentication no/g' /etc/ssh/sshd_config
#  sshd-install-pm 
RUN rsync -az /root/.ssh/ /home/REPLACE_USERNAME/.ssh/
#  sshconfig-install-man
RUN mkdir -p ~/.ssh
RUN chmod 700 ~/.ssh
RUN echo "StrictHostKeyChecking=no" > /home/REPLACE_USERNAME/.ssh/config
RUN echo "UserKnownHostsFile=/dev/null" >> /home/REPLACE_USERNAME/.ssh/config
RUN chmod 600 /home/REPLACE_USERNAME/.ssh/config
RUN chown -R REPLACE_USERNAME:REPLACE_USERNAME /home/REPLACE_USERNAME/.ssh/config
#  sshconfig-install-man
RUN chown -R REPLACE_USERNAME:REPLACE_USERNAME /home/REPLACE_USERNAME/
USER REPLACE_USERNAME
WORKDIR /home/REPLACE_USERNAME/
RUN git clone https://github.com/ibmcb/cbtool.git ; cd cbtool ; git checkout REPLACE_BRANCH
#  gmetad-python-install-git
RUN mkdir -p /home/REPLACE_USERNAME/cbtool/3rd_party
WORKDIR /home/REPLACE_USERNAME/cbtool/3rd_party
RUN git clone https://github.com/ibmcb/monitor-core.git
#  gmetad-python-install-git
#  pyhtml-install-git
WORKDIR /home/REPLACE_USERNAME/cbtool/3rd_party
RUN git clone https://github.com/ibmcb/HTML.py.git
WORKDIR /home/REPLACE_USERNAME/cbtool/3rd_party/HTML.py
RUN sudo python setup.py install
#  pyhtml-install-git
WORKDIR /home/REPLACE_USERNAME
USER root
RUN chown -R REPLACE_USERNAME:REPLACE_USERNAME /home/REPLACE_USERNAME
