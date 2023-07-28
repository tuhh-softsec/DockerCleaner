FROM hurricane/dockergui:x11rdp
#  FROM hurricane/dockergui:xvnc
MAINTAINER David Coppit <david@coppit.org>
ENV APP_NAME="Filebot" \
    WIDTH="1280" \
    HEIGHT="720" \
    TERM="xterm"
#   Use baseimage-docker's init system
CMD ["/sbin/my_init"]
ENV DEBIAN_FRONTEND="noninteractive"
COPY dpkg-excludes /etc/dpkg/dpkg.cfg.d/excludes
RUN true \
 && mkdir /files \
 && chmod a+rwX /files \
 && echo "force-unsafe-io" > /etc/dpkg/dpkg.cfg.d/02apt-speedup \
 && echo "Acquire::http {No-Cache=True;};" > /etc/apt/apt.conf.d/no-cache \
 && add-apt-repository ppa:webupd8team/java
RUN true \
 && echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections \
 && apt-get update \
 && (apt-get install --no-install-recommends 'oracle-java8-installer=8u161-1~webupd8~0' -qy || true )
RUN true \
 && sed -i 's|JAVA_VERSION=8u151|JAVA_VERSION=8u161|' /var/lib/dpkg/info/oracle-java8-installer.* \
 && sed -i 's|PARTNER_URL=http://download.oracle.com/otn-pub/java/jdk/8u151-b12/e758a0de34e24606bca991d704f6dcbf/|PARTNER_URL=http://download.oracle.com/otn-pub/java/jdk/8u161-b12/2f38c3b165be4555a1fa6e98c45e0808/|' /var/lib/dpkg/info/oracle-java8-installer.* \
 && sed -i 's|SHA256SUM_TGZ="c78200ce409367b296ec39be4427f020e2c585470c4eed01021feada576f027f"|SHA256SUM_TGZ="6dbc56a0e3310b69e91bb64db63a485bd7b6a8083f08e48047276380a0e2021e"|' /var/lib/dpkg/info/oracle-java8-installer.* \
 && sed -i 's|J_DIR=jdk1.8.0_151|J_DIR=jdk1.8.0_161|' /var/lib/dpkg/info/oracle-java8-installer.* \
 && apt-get install --no-install-recommends 'oracle-java8-installer=8u161-1~webupd8~0' -qy
RUN true \
 && apt-get install --no-install-recommends mediainfo libchromaprint-tools -qy \
 && apt-get install --no-install-recommends libxslt1-dev libglapi-mesa-lts-xenial libgl1-mesa-glx-lts-xenial -qy \
 && apt-get install --no-install-recommends python3-setuptools -qy \
 && easy_install3 watchdog \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /usr/share/man /usr/share/groff /usr/share/info /usr/share/lintian /usr/share/linda /var/cache/man \
 && ((find /usr/share/doc -depth -type f ! -name copyright | xargs rm || true ) ) \
 && ((find /usr/share/doc -empty | xargs rmdir || true ) )
VOLUME ["/media", "/input", "/output", "/config"]
ENV USER_ID="0"
ENV GROUP_ID="0"
ENV UMASK="0000"
EXPOSE 3389/tcp 8080/tcp
#   Set the locale, to support files that have non-ASCII characters
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
COPY startapp.sh /
RUN true \
 && mkdir -p /var/cache/tomcat7 /tmp/tomcat7-tomcat7-tmp /var/lib/tomcat7/work/Catalina/localhost \
 && ln -s /var/lib/tomcat7/common /usr/share/tomcat7/common \
 && ln -s /var/lib/tomcat7/server /usr/share/tomcat7/server \
 && ln -s /var/lib/tomcat7/shared /usr/share/tomcat7/shared \
 && wget --no-check-certificate -q -O /files/filebot.deb 'https://sourceforge.net/projects/filebot/files/filebot/FileBot_4.7.9/filebot_4.7.9_amd64.deb' \
 && dpkg -i /files/filebot.deb \
 && rm /files/filebot.deb \
 && sed -i 's/java /java -Dsun.java2d.xrender=false /' /usr/bin/filebot \
 && wget --no-check-certificate -q -O /files/runas.sh 'https://raw.githubusercontent.com/coppit/docker-inotify-command/1d4b941873b670525fd159dcb9c01bb2570b0565/runas.sh' \
 && chmod +x /files/runas.sh \
 && wget --no-check-certificate -q -O /files/monitor.py 'https://raw.githubusercontent.com/coppit/docker-inotify-command/c9e9c8b980d3a5ba4abfe7c1b069f684a56be6d2/monitor.py' \
 && chmod +x /files/monitor.py
#   Add scripts. Make sure everything is executable by $USER_ID
COPY pre-run.sh filebot.sh filebot.conf /files/
RUN chmod a+x /files/pre-run.sh
RUN chmod a+w /files/filebot.conf
COPY 50_configure_filebot.sh /etc/my_init.d/
RUN mkdir /etc/service/filebot
COPY monitor.sh /etc/service/filebot/run
RUN chmod +x /etc/service/filebot/run
RUN mkdir /etc/service/filebot-ui
COPY startapp.sh /etc/service/filebot-ui/run
RUN chmod +x /etc/service/filebot-ui/run
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
