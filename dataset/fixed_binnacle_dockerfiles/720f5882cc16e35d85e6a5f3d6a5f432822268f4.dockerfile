FROM centos:centos7.6.1810
MAINTAINER FIWARE Orion Context Broker Team. Telefónica I+D
ENV ORION_USER="orion"
ENV GIT_REV_ORION="master"
ENV CLEAN_DEV_TOOLS="1"
WORKDIR /opt
RUN adduser --comment "${ORION_USER}" ${ORION_USER} \
 && yum -y install epel-release \
 && yum -y install boost-devel bzip2 cmake gnutls-devel libgcrypt-devel libcurl-devel openssl-devel libuuid-devel make nc git gcc-c++ scons tar which cyrus-sasl-devel \
 && curl -kOL http://ftp.gnu.org/gnu/libmicrohttpd/libmicrohttpd-0.9.48.tar.gz \
 && tar xvf libmicrohttpd-0.9.48.tar.gz \
 && cd libmicrohttpd-0.9.48 \
 && ./configure --disable-messages --disable-postprocessor --disable-dauth \
 && make \
 && make install \
 && ldconfig \
 && curl -kOL https://github.com/mongodb/mongo-cxx-driver/archive/legacy-1.1.2.tar.gz \
 && tar xfz legacy-1.1.2.tar.gz \
 && cd mongo-cxx-driver-legacy-1.1.2 \
 && scons --use-sasl-client --ssl \
 && scons install --prefix=/usr/local --use-sasl-client --ssl \
 && curl -kOL https://github.com/miloyip/rapidjson/archive/v1.0.2.tar.gz \
 && tar xfz v1.0.2.tar.gz \
 && mv rapidjson-1.0.2/include/rapidjson/ /usr/local/include \
 && git clone https://github.com/telefonicaid/fiware-orion \
 && cd fiware-orion \
 && git checkout ${GIT_REV_ORION} \
 && make \
 && make install \
 && strip /usr/bin/contextBroker \
 && mkdir -p /var/log/contextBroker \
 && mkdir -p /var/run/contextBroker \
 && chown ${ORION_USER} /var/log/contextBroker \
 && chown ${ORION_USER} /var/run/contextBroker \
 && cd /opt \
 && if [ ${CLEAN_DEV_TOOLS} -eq 0 ] ; then yum clean all \
 && exit 0 ; fi \
 && rm -rf /opt/libmicrohttpd-0.9.48.tar.gz /usr/local/include/microhttpd.h /usr/local/lib/libmicrohttpd.* /opt/libmicrohttpd-0.9.48 /opt/legacy-1.1.2.tar.gz /opt/mongo-cxx-driver-legacy-1.1.2 /usr/local/include/mongo /usr/local/lib/libmongoclient.a /opt/rapidjson-1.0.2 /opt/v1.0.2.tar.gz /usr/local/include/rapidjson /opt/fiware-orion /usr/share/cracklib /usr/share/i18n /usr/{lib,lib64}/gconv \
 && yum -y erase git perl* rsync cmake libarchive gcc-c++ cloog-ppl cpp gcc glibc-devel glibc-headers kernel-headers libgomp libstdc++-devel mpfr ppl scons boost-devel libcurl-devel gnutls-devel libgcrypt-devel clang llvm llvm-libs CUnit-devel CUnit autoconf automake m4 libidn-devel boost-wave boost-serialization boost-python boost-iostreams boost boost-date-time boost-test boost-graph boost-signals boost-program-options boost-math openssh openssh-clients libedit hwdata dbus-glib fipscheck* *devel sysvinit-tools \
 && rpm -qa groff | xargs -r rpm -e --nodeps \
 && yum clean all \
 && rm -rf /var/lib/yum/yumdb \
 && rm -rf /var/lib/yum/history \
 && rpm -vv --rebuilddb \
 && find /usr/share/locale -mindepth 1 -maxdepth 1 ! -name 'en_US' ! -name 'locale.alias' | xargs -r rm -r \
 && bash -c 'localedef --list-archive | grep -v -e "en_US" | xargs localedef --delete-from-archive' \
 && /bin/cp -f /usr/lib/locale/locale-archive /usr/lib/locale/locale-archive.tmpl \
 && build-locale-archive \
 && rm -f /var/log/*log
WORKDIR /
ENTRYPOINT ["/usr/bin/contextBroker", "-fg", "-multiservice", "-ngsiv1Autocast"]
EXPOSE 1026/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
