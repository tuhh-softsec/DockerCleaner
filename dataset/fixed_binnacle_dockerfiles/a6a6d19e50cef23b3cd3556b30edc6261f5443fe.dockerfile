FROM debian:stretch
ARG BUILD_DATE
ARG VCS_REF
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg2=2.1.18-8~deb9u4 dirmngr=2.1.18-8~deb9u4 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists
COPY kali.pub /etc/apt/kali.pub
RUN echo "deb http://http.kali.org/kali kali-rolling main contrib non-free" > /etc/apt/sources.list.d/kali.list \
 && echo "deb-src http://http.kali.org/kali kali-rolling main contrib non-free" >> /etc/apt/sources.list.d/kali.list \
 && apt-key add /etc/apt/kali.pub \
 && apt-get update \
 && apt-get install --no-install-recommends less=481-2.1 vim=2:8.0.0197-4+deb9u7 build-essential=12.3 libreadline-dev=7.0-3 libssl-dev=1.1.0l-1~deb9u6 libpq5=9.6.24-0+deb9u1 libpq-dev=9.6.24-0+deb9u1 libreadline5=5.2+dfsg-3+b1 libsqlite3-dev=3.16.2-5+deb9u3 libpcap-dev=1.8.1-3+deb9u1 subversion=1.9.5-1+deb9u6 git-core=1:2.11.0-3+deb9u7 autoconf=2.69-10 pgadmin3=1.22.2-1 curl=7.52.1-5+deb9u16 zlib1g-dev=1:1.2.8.dfsg-5+deb9u1 libxml2-dev=2.9.4+dfsg1-2.2+deb9u7 libxslt1-dev=1.1.29-2.1+deb9u2 xtightvncviewer=1:1.3.9-9+deb9u1 libyaml-dev=0.1.7-2 ruby=1:2.3.3 ruby-dev=1:2.3.3 nmap=7.40-1 beef-xss mitmproxy=0.18.2-6+deb9u2 python-pefile=2016.3.28-4 net-tools=1.60+git20161116.90da8a0-1 iputils-ping=3:20161105-1 iptables=1.6.0+snapshot20161117-6 sqlmap=1.1-2 bettercap bdfproxy rsync=3.1.2-1+deb9u3 enum4linux openssh-client=1:7.4p1-10+deb9u7 mfoc mfcuk libnfc-bin=1.7.1-4+b1 hydra=8.3-3 nikto weevely netcat-traditional=1.10-41+b1 aircrack-ng=1:1.2-0~rc4-2 pyrit cowpatty pciutils=1:3.5.2-1 kmod=23-2 wget=1.18-5+deb9u3 unicornscan ftp=0.17-34 wfuzz=2.1.4-1 python-pip=9.0.1-2+deb9u2 moreutils=0.60-1 upx john=1.8.0-2+b1 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists \
 && curl https://github.com/brimstone/gobuster/releases/download/1.3-opt/gobuster -Lo /usr/bin/gobuster \
 && chmod 755 /usr/bin/gobuster
#   I'm trying to split up this layer so it's more palatable to download
RUN apt-get update \
 && apt-get install --no-install-recommends burpsuite openjdk-8-jre=8u332-ga-1~deb9u1 zaproxy exploitdb -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists
RUN git clone https://github.com/brimstone/SecLists /pentest/seclists --depth 1 \
 && rm -rf /pentest/seclists/.git \
 && git clone https://github.com/FireFart/msfpayloadgenerator /pentest/msfpayloadgenerator --depth 1 \
 && rm -rf /pentest/msfpayloadgenerator/.git \
 && wget https://github.com/Charliedean/NetcatUP/raw/master/netcatup.sh -O /bin/netcatup.sh \
 && git clone https://github.com/derv82/wifite /opt/wifite --depth 1 \
 && ln -s /opt/wifite/wifite.py /sbin/wifite
#   empire
RUN apt-get update \
 && apt-get install --no-install-recommends python-iptools python-netifaces=0.10.4-0.1+b2 python-pydispatch=2.0.5-1 python-zlib-wrapper python-m2crypto=0.24.0-1.1 python-macholib=1.8~dfsg-1 python-xlrd=1.0.0-1 python-xlutils python-dropbox python-pyminifier -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists \
 && git clone -b dev https://github.com/EmpireProject/Empire /pentest/empire \
 && cd /pentest/empire \
 && printf "\n" | python setup/setup_database.py \
 && chmod 755 empire \
 && mkdir lib/modules/python/brimstone
COPY empire/* /pentest/empire/lib/modules/python/brimstone
#   pupy
RUN apt-get update \
 && apt-get install --no-install-recommends python-dev=2.7.13-2 python-setuptools=33.1.1-1 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists \
 && git clone --recursive https://github.com/n1nj4sec/pupy /pentest/pupy \
 && cd /pentest/pupy \
 && cd pupy \
 && pip install -r requirements.txt \
 && cd /pentest/pupy/pupy \
 && wget https://github.com/n1nj4sec/pupy/releases/download/latest/payload_templates.txz \
 && tar xvf payload_templates.txz \
 && rm payload_templates.txz
#   msf python
RUN apt-get update \
 && apt-get install --no-install-recommends python3-pip=9.0.1-2+deb9u2 python3-setuptools=33.1.1-1 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists \
 && pip3 install pymetasploit3
COPY bashrc /root/.bashrc
COPY lists /pentest/lists
COPY bin/* /usr/local/bin/
COPY share /pentest/share
COPY ssh_config /etc/ssh/ssh_config
RUN chown root:root /etc/ssh/ssh_config \
 && mkdir /root/.ssh \
 && chmod 700 /root/.ssh
WORKDIR /pentest
LABEL org.label-schema.build-date="$BUILD_DATE" \
      org.label-schema.vcs-url="https://github.com/brimstone/docker-kali" \
      org.label-schema.vcs-ref="$VCS_REF" \
      org.label-schema.schema-version="1.0.0-rc1"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
