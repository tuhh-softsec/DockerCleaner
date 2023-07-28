#  airgeddon Dockerfile
#  Base image
FROM parrotsec/parrot:latest
#  Credits & Data
LABEL name="airgeddon" \
      author="v1s1t0r <v1s1t0r.1s.h3r3@gmail.com>" \
      maintainer="OscarAkaElvis <oscar.alfonso.diaz@gmail.com>" \
      description="This is a multi-use bash script for Linux systems to audit wireless networks."
#  Env vars
ENV AIRGEDDON_URL="https://github.com/v1s1t0r1sh3r3/airgeddon.git"
ENV HASHCAT2_URL="https://github.com/v1s1t0r1sh3r3/hashcat2.0.git"
ENV BETTERCAP162_URL="https://github.com/v1s1t0r1sh3r3/bettercap1.6.2.git"
ENV DEBIAN_FRONTEND="noninteractive"
#  Update system
RUN :
#  Set locales
RUN (apt-get update ;apt-get install --no-install-recommends locales=2.37-0ubuntu2 -y ) \
 && locale-gen en_US.UTF-8 \
 && sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen \
 && echo 'LANG="en_US.UTF-8"' > /etc/default/locale \
 && dpkg-reconfigure --frontend=noninteractive locales \
 && update-locale LANG=en_US.UTF-8
#  Env vars for locales
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
#  Install airgeddon essential tools
RUN (apt-get update ;apt-get install --no-install-recommends gawk=1:5.2.1-2 net-tools=2.10-0.1ubuntu3 wireless-tools=30~pre9-13.1ubuntu4 iw=5.19-1 aircrack-ng=1:1.7-5 xterm=379-1ubuntu1 iproute2=6.1.0-1ubuntu2 -y )
#  Install airgeddon internal tools
RUN (apt-get update ;apt-get install --no-install-recommends ethtool=1:6.1-1 pciutils=1:3.9.0-4 usbutils=1:014-1build1 rfkill=2.38.1-4ubuntu1 x11-utils=7.7+5build2 wget=1.21.3-1ubuntu1 ccze=0.2.1-7 x11-xserver-utils=7.7+9build1 -y )
#  Install update tools
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.88.1-7ubuntu1 git=1:2.39.2-1ubuntu1 -y )
#  Install airgeddon optional tools
RUN (apt-get update ;apt-get install --no-install-recommends crunch=3.6-3 hashcat=6.2.6+ds1-1 mdk4=4.2-3ubuntu1 hostapd=2:2.10-12 lighttpd=1.4.67-1ubuntu2 iptables=1.8.7-1ubuntu7 nftables=1.0.6-2 ettercap-text-only=1:0.8.3.1-11 sslstrip isc-dhcp-server=4.4.3-P1-1ubuntu1 dsniff=2.4b1+debian-31 reaver=1.6.5-1 bully=1.4.00-2ubuntu1 pixiewps=1.4.2-5 expect=5.45.4-2build1 hostapd-wpe asleap john=1.8.0-4ubuntu3 -y )
#  Install needed Ruby gems
RUN (apt-get update ;apt-get install --no-install-recommends beef-xss bettercap=2.32.0-1ubuntu1 ruby-packetfu=1.1.11-2.1 ruby-colorize=0.8.1-1.1 ruby-net-dns=0.9.1-2 ruby-em-proxy ruby-network-interface -y )
#  Env var for display
ENV DISPLAY=":0"
#  Create volume dir for external files
RUN mkdir /io
VOLUME /io
#  Set workdir
WORKDIR /opt/
#  airgeddon install method 1 (only one method can be used, other must be commented)
#  Install airgeddon (Docker Hub automated build process)
RUN mkdir airgeddon
COPY . /opt/airgeddon
#  airgeddon install method 2 (only one method can be used, other must be commented)
#  Install airgeddon (manual image build)
#  Uncomment git clone line and one of the ENV vars to select branch (master->latest, dev->beta)
#  ENV BRANCH="master"
#  ENV BRANCH="dev"
#  RUN git clone -b ${BRANCH} ${AIRGEDDON_URL}
#  Remove auto update
RUN sed -i 's|AIRGEDDON_AUTO_UPDATE=true|AIRGEDDON_AUTO_UPDATE=false|' airgeddon/.airgeddonrc
#  Force use of iptables
RUN sed -i 's|AIRGEDDON_FORCE_IPTABLES=false|AIRGEDDON_FORCE_IPTABLES=true|' airgeddon/.airgeddonrc
#  Make bash script files executable
RUN chmod +x airgeddon/*.sh
#  Downgrade Hashcat
RUN git clone ${HASHCAT2_URL} \
 && cp /opt/hashcat2.0/hashcat /usr/bin/ \
 && chmod +x /usr/bin/hashcat
#  Downgrade Bettercap
RUN git clone ${BETTERCAP162_URL} \
 && dpkg -i /opt/bettercap1.6.2/bettercap_1.6.2-0parrot1_all.deb
#  Clean packages
RUN apt-get clean \
 && apt-get autoclean \
 && apt-get autoremove -y
#  Clean files
RUN rm -rf /opt/airgeddon/imgs > /dev/null 2>&1 \
 && rm -rf /opt/airgeddon/.github > /dev/null 2>&1 \
 && rm -rf /opt/airgeddon/.editorconfig > /dev/null 2>&1 \
 && rm -rf /opt/airgeddon/CONTRIBUTING.md > /dev/null 2>&1 \
 && rm -rf /opt/airgeddon/CODE_OF_CONDUCT.md > /dev/null 2>&1 \
 && rm -rf /opt/airgeddon/pindb_checksum.txt > /dev/null 2>&1 \
 && rm -rf /opt/airgeddon/Dockerfile > /dev/null 2>&1 \
 && rm -rf /opt/airgeddon/binaries > /dev/null 2>&1 \
 && rm -rf /opt/hashcat2.0 > /dev/null 2>&1 \
 && rm -rf /opt/bettercap1.6.2 > /dev/null 2>&1 \
 && rm -rf /tmp/* > /dev/null 2>&1 \
 && rm -rf /var/lib/apt/lists/* > /dev/null 2>&1
#  Expose BeEF control panel port
EXPOSE 3000/tcp
#  Start command (launching airgeddon)
CMD ["/bin/bash", "-c", "/opt/airgeddon/airgeddon.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
