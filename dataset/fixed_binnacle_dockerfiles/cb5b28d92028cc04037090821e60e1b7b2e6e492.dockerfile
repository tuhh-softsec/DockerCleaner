FROM docker:18.05-rc
RUN apk add curl=7.88.1-r1 ca-certificates=20220614-r4 bash=5.2.15-r0 sudo=1.9.12_p2-r1 openssl=3.0.8-r3 --no-cache
RUN curl https://raw.githubusercontent.com/kubernetes/helm/master/scripts/get | bash
RUN curl https://storage.googleapis.com/kubernetes-release/release/$( curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt ;)/bin/linux/amd64/kubectl > /usr/local/bin/kubectl
ENV HOME="/config"
RUN set +e \
 && chmod +x /usr/local/bin/kubectl
#  kubectl version --client
RUN apk add git=2.38.4-r1 --no-cache
#   Java Version and other ENV
ENV JAVA_VERSION_MAJOR="8" \
    JAVA_VERSION_MINOR="131" \
    JAVA_VERSION_BUILD="11" \
    JAVA_PACKAGE="jdk" \
    JAVA_JCE="standard" \
    JAVA_HOME="/opt/jdk" \
    PATH="${PATH}:/opt/jdk/bin" \
    GLIBC_VERSION="2.23-r3" \
    LANG="C.UTF-8"
#   do all in one step
RUN set -ex \
 && apk upgrade --update \
 && apk add libstdc++=12.2.1_git20220924-r4 curl=7.88.1-r1 ca-certificates=20220614-r4 bash=5.2.15-r0 --update \
 && for pkg in glibc-${GLIBC_VERSION} glibc-bin-${GLIBC_VERSION} glibc-i18n-${GLIBC_VERSION}; do curl -sSL https://github.com/andyshinn/alpine-pkg-glibc/releases/download/${GLIBC_VERSION}/${pkg}.apk -o /tmp/${pkg}.apk ; done \
 && apk add /tmp/*.apk --allow-untrusted \
 && rm -v /tmp/*.apk \
 && (/usr/glibc-compat/bin/localedef --force --inputfile POSIX --charmap UTF-8 C.UTF-8 || true ) \
 && echo "export LANG=C.UTF-8" > /etc/profile.d/locale.sh \
 && /usr/glibc-compat/sbin/ldconfig /lib /usr/glibc-compat/lib \
 && mkdir /opt \
 && curl -jksSLH "Cookie: oraclelicense=accept-securebackup-cookie" -o /tmp/java.tar.gz http://download.oracle.com/otn-pub/java/jdk/${JAVA_VERSION_MAJOR}u${JAVA_VERSION_MINOR}-b${JAVA_VERSION_BUILD}/d54c1d3a095b4ff2b6607d096fa80163/${JAVA_PACKAGE}-${JAVA_VERSION_MAJOR}u${JAVA_VERSION_MINOR}-linux-x64.tar.gz \
 && gunzip /tmp/java.tar.gz \
 && tar -C /opt -xf /tmp/java.tar \
 && ln -s /opt/jdk1.${JAVA_VERSION_MAJOR}.0_${JAVA_VERSION_MINOR} /opt/jdk \
 && if [ "${JAVA_JCE}" == "unlimited" ] ; then echo "Installing Unlimited JCE policy" >&2 \
 && curl -jksSLH "Cookie: oraclelicense=accept-securebackup-cookie" -o /tmp/jce_policy-${JAVA_VERSION_MAJOR}.zip http://download.oracle.com/otn-pub/java/jce/${JAVA_VERSION_MAJOR}/jce_policy-${JAVA_VERSION_MAJOR}.zip \
 && cd /tmp \
 && unzip /tmp/jce_policy-${JAVA_VERSION_MAJOR}.zip \
 && cp -v /tmp/UnlimitedJCEPolicyJDK8/*.jar /opt/jdk/jre/lib/security ; fi \
 && sed -i s/#networkaddress.cache.ttl=-1/networkaddress.cache.ttl=10/ $JAVA_HOME/jre/lib/security/java.security \
 && apk del curl glibc-i18n \
 && rm -rf /opt/jdk/*src.zip /opt/jdk/lib/missioncontrol /opt/jdk/lib/visualvm /opt/jdk/lib/*javafx* /opt/jdk/jre/plugin /opt/jdk/jre/bin/javaws /opt/jdk/jre/bin/jjs /opt/jdk/jre/bin/orbd /opt/jdk/jre/bin/pack200 /opt/jdk/jre/bin/policytool /opt/jdk/jre/bin/rmid /opt/jdk/jre/bin/rmiregistry /opt/jdk/jre/bin/servertool /opt/jdk/jre/bin/tnameserv /opt/jdk/jre/bin/unpack200 /opt/jdk/jre/lib/javaws.jar /opt/jdk/jre/lib/deploy* /opt/jdk/jre/lib/desktop /opt/jdk/jre/lib/*javafx* /opt/jdk/jre/lib/*jfx* /opt/jdk/jre/lib/amd64/libdecora_sse.so /opt/jdk/jre/lib/amd64/libprism_*.so /opt/jdk/jre/lib/amd64/libfxplugins.so /opt/jdk/jre/lib/amd64/libglass.so /opt/jdk/jre/lib/amd64/libgstreamer-lite.so /opt/jdk/jre/lib/amd64/libjavafx*.so /opt/jdk/jre/lib/amd64/libjfx*.so /opt/jdk/jre/lib/ext/jfxrt.jar /opt/jdk/jre/lib/ext/nashorn.jar /opt/jdk/jre/lib/oblique-fonts /opt/jdk/jre/lib/plugin.jar /tmp/* /var/cache/apk/* \
 && echo 'hosts: files mdns4_minimal [NOTFOUND=return] dns mdns4' >> /etc/nsswitch.conf
#   EOF
RUN apk add curl=7.88.1-r1 --update
RUN curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > /usr/bin/lein \
 && chmod u+x /usr/bin/lein \
 && lein \
 && echo 'ok'
RUN apk add python py-pip py-cffi py-cryptography --update \
 && pip install pip==23.1 --upgrade \
 && apk add gcc=12.2.1_git20220924-r4 libffi-dev=3.4.4-r0 python-dev linux-headers=5.19.5-r0 musl-dev=1.2.3-r4 openssl-dev=3.0.8-r3 --virtual build-deps \
 && pip install gsutil==5.23 \
 && apk del build-deps \
 && rm -rf /var/cache/apk/*
RUN apk add rlwrap=0.46.1-r0 --update-cache --repository http://dl-3.alpinelinux.org/alpine/edge/testing/ --allow-untrusted
RUN cd /tmp \
 && curl -O https://download.clojure.org/install/linux-install-1.9.0.375.sh \
 && chmod +x linux-install-1.9.0.375.sh \
 && ./linux-install-1.9.0.375.sh
COPY target/ci3.jar /var/ci3.jar
RUN mkdir /workspace
WORKDIR /workspace
ENV LEIN_ROOT="1"
ENV BOTO_CONFIG="/root/.boto"
COPY entrypoint /usr/local/bin/
RUN chmod u+x /usr/local/bin/entrypoint
ENV DOCKER_API_VERSION="1.23"
ENTRYPOINT ["/usr/local/bin/entrypoint"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
