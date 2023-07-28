FROM REPLACE_BASE_VANILLA_UBUNTU
ENV DEBIAN_FRONTEND="noninteractive"
RUN :
RUN useradd -ms /bin/bash REPLACE_USERNAME
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
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.39.2-1ubuntu1 bc=1.07.1-3build1 -y )
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
RUN ln -s /usr/sbin/rsyslogd /usr/local/bin/rsyslogd
RUN mkdir -p /var/log/cloudbench
#   rsyslog-install-pm
#   rsyslog-filter-pm
RUN mkdir -p /var/log/cloudbench
RUN sed -i -e "s/#$ModLoad imudp/$ModLoad imudp/g" /etc/rsyslog.conf ; sed -i -e 's^#module(load="imudp")^module(load="imudp")^g' /etc/rsyslog.conf
RUN sed -i "s/#$UDPServerRun.*/$UDPServerRun $METRIC_STORE_PORT/g" /etc/rsyslog.conf
RUN bash -c "echo -e \"local5.* \t\t\t\t /var/log/cloudbench/remote.log\" >> /etc/rsyslog.conf"
RUN bash -c "echo -e \"local6.* \t\t\t\t /var/log/cloudbench/local.log\" >> /etc/rsyslog.conf"
#  service_restart_enable rsyslog
#   rsyslog-filter-pm
#   apache-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends apache2=2.4.55-1ubuntu2 -y )
#   apache-install-pm
#   redis-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends redis-server=5:7.0.8-4 -y )
RUN sed -i "s/.*bind 127.0.0.1/bind 0.0.0.0/" /etc/redis/redis.conf
#   redis-install-pm
RUN sed -i "s/.*port.*/port $OBJECTSTORE_PORT/" /etc/redis/redis.conf
#   mongodb-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends mongodb -y )
RUN sed -i "s/.*bind_ip.*/bind_ip=0.0.0.0/" /etc/mongodb.conf
#   mongodb-install-pm
RUN sed -i "s/.*port.*/port = $METRICSTORE_PORT/" /etc/mongodb.conf
RUN (apt-get update ;apt-get install --no-install-recommends sudo=1.9.13p1-1ubuntu2 -y )
RUN echo "REPLACE_USERNAME ALL=(ALL:ALL) NOPASSWD:ALL" >> /etc/sudoers
#   pylibvirt-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends python-libvirt -y )
#   pylibvirt-install-pm
#   pypureomapi-install-pip
RUN pip install pypureomapi==0.8 --upgrade
#   pypureomapi-install-pip
#   python-devel-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends python-dev libffi-dev=3.4.4-1 libssl-dev=3.0.8-1ubuntu1 libxml2-dev=2.9.14+dfsg-1.1build2 libxslt1-dev=1.1.35-1 libjpeg8-dev=8c-2ubuntu11 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 -y )
#   python-devel-install-pm 
#   python-prettytable-install-pip
RUN pip install prettytable==3.7.0 docutils==0.19 --upgrade
#   python-prettytable-install-pip
#   python-daemon-install-pip
RUN pip install python-daemon==3.0.1 --upgrade
#   python-daemon-install-pip
#   python-twisted-install-pip
RUN pip install twisted==22.10.0 service_identity==21.1.0 --upgrade
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
#   ruamelyaml-install-pip
RUN pip install ruamel.yaml==0.17.21 --upgrade
#   ruamelyaml-install-pip
#   urllib3-install-pip
RUN pip install urllib3[secure] --upgrade
#   urllib3-install-pip
#   jq-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends jq=1.6-2.1ubuntu3 -y )
#   jq-install-pm
#   httplib2shim-install-pip
RUN pip install httplib2shim==0.0.3 --upgrade
#   httplib2shim-install-pip
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
#   novaclient-install-pip
RUN pip install pbr==5.11.1 --upgrade
RUN pip install netifaces==0.11.0 --upgrade
RUN pip install python-novaclient==9.1.1 --upgrade
#   novaclient-install-pip
#   neutronclient-install-pip
RUN pip install python-neutronclient==6.5.0 --upgrade
#   neutronclient-install-pip
#   cinderclient-install-pip
RUN pip install python-cinderclient==3.2.0 --upgrade
#   cinderclient-install-pip
#   glanceclient-install-pip
RUN pip install python-glanceclient==2.8.0 --upgrade
#   glanceclient-install-pip
#   softlayer-install-pip
RUN pip install softlayer==6.1.6 --upgrade
#   softlayer-install-pip
#   boto-install-pip
RUN pip install boto==2.49.0 --upgrade
#   boto-install-pip
#   libcloud-install-pip
RUN pip install apache-libcloud==3.7.0 --upgrade
#   libcloud-install-pip
#   pygce-install-pip
RUN pip install gcloud==0.18.3 google-api-python-client==2.85.0 --upgrade
#   pygce-install-pip
#   pydocker-install-pip
RUN pip install docker-py==1.8.1 wget==3.2 --upgrade
#   pydocker-install-pip
#   pylxd-install-pip
RUN pip install pylxd==2.3.1 --upgrade
#   pylxd-install-pip
#   pykube-install-pip
RUN pip install pykube==0.15.0 --upgrade
#   pykube-install-pip
#   R-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends r-base-core=4.2.2.20221110-2build1 -y )
#   R-install-pm
#   rrdtool-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends rrdtool=1.7.2-4ubuntu5 -y )
#   rrdtool-install-pm
#   python-rrdtool-install-pm
RUN (apt-get update ;apt-get install --no-install-recommends python-rrdtool -y )
#   python-rrdtool-install-pm
#   python-dateutil-install-pip
RUN pip install python-dateutil==2.8.2 --upgrade
#   python-dateutil-install-pip
#   python-pillow-install-pip
RUN pip install Pillow==9.5.0 --upgrade
#   python-pillow-install-pip
#   python-jsonschema-install-pip
RUN pip install jsonschema==4.17.3 --upgrade
#   python-jsonschema-install-pip
COPY get_my_ips.sh /usr/local/bin/getmyips
COPY gucn.sh /usr/local/bin/gucn
RUN chmod +x /usr/local/bin/getmyips ; chmod +x /usr/local/bin/gucn
USER REPLACE_USERNAME
#   gcloud-install-man
ENV CLOUDSDK_CORE_DISABLE_PROMPTS="1"
RUN curl https://sdk.cloud.google.com | bash
RUN sudo ln -s /home/REPLACE_USERNAME/google-cloud-sdk/bin/gcloud /usr/local/bin/gcloud
#   gcloud-install-man
WORKDIR /home/REPLACE_USERNAME/
#   gmetad-python-install-git
RUN mkdir -p /home/REPLACE_USERNAME/cbtool/3rd_party/workload
RUN cp____-f____/home/REPLACE_USERNAME/cbtool/util/manually_download_files.txt____/home/REPLACE_USERNAME/cbtool/3rd_party/workload ; /bin/true
WORKDIR /home/REPLACE_USERNAME/cbtool/3rd_party
RUN git clone https://github.com/ibmcb/monitor-core.git
#   gmetad-python-install-git
#   pyhtml-install-git
WORKDIR /home/REPLACE_USERNAME/cbtool/3rd_party
RUN git clone https://github.com/ibmcb/HTML.py.git
WORKDIR /home/REPLACE_USERNAME/cbtool/3rd_party/HTML.py
RUN sudo python setup.py install
#   pyhtml-install-git
#   bootstrap-install-git
WORKDIR /home/REPLACE_USERNAME/cbtool/3rd_party
RUN git clone https://github.com/ibmcb/bootstrap.git
#   bootstrap-install-git
#   bootstrap-wizard-install-git
WORKDIR /home/REPLACE_USERNAME/cbtool/3rd_party
RUN git clone https://github.com/ibmcb/Bootstrap-Wizard.git
#   bootstrap-wizard-install-git
#   streamprox-install-git
WORKDIR /home/REPLACE_USERNAME/cbtool/3rd_party
RUN git clone https://github.com/ibmcb/StreamProx.git
#   streamprox-install-git
#   d3-install-git
WORKDIR /home/REPLACE_USERNAME/cbtool/3rd_party
RUN git clone https://github.com/ibmcb/d3.git
#   d3-install-git
WORKDIR /home/REPLACE_USERNAME
RUN mkdir -p /home/REPLACE_USERNAME/cbtool/lib/clouds/
RUN mkdir -p /home/REPLACE_USERNAME/cbtool/configs/templates/
COPY cloud_definitions.txt /home/REPLACE_USERNAME/cbtool/configs/
RUN sudo chown -R REPLACE_USERNAME:REPLACE_USERNAME /home/REPLACE_USERNAME
RUN mv /home/REPLACE_USERNAME/cbtool /home/REPLACE_USERNAME/cbtooltmp
# Please add your HEALTHCHECK here!!!
