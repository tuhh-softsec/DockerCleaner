FROM python:2.7-slim
ENV RASA_NLU_DOCKER="YES" \
    RASA_NLU_HOME="/app" \
    RASA_NLU_PYTHON_PACKAGES="/usr/local/lib/python2.7/dist-packages"
#   Run updates, install basics and cleanup
#   - build-essential: Compile specific dependencies
#   - git-core: Checkout git repos
RUN apt-get update -qq \
 && apt-get install --no-install-recommends build-essential=12.6 git-core openssl=1.1.1n-0+deb10u4 libssl-dev=1.1.1n-0+deb10u4 libffi6=3.2.1-9 libffi-dev=3.2.1-9 curl=7.64.0-4+deb10u5 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   use bash always
RUN rm /bin/sh \
 && ln -s /bin/bash /bin/sh
#  # install java stuff
RUN echo "deb http://http.debian.net/debian jessie-backports main" > /etc/apt/sources.list.d/jessie-backports.list
RUN apt-get update \
 && apt-get install --no-install-recommends bzip2=1.0.6-9.2~deb10u2 unzip=6.0-23+deb10u3 xz-utils=5.2.4-1+deb10u1 -y \
 && rm -rf /var/lib/apt/lists/*
#   Default to UTF-8 file.encoding
ENV LANG="C.UTF-8"
#   add a simple script that can auto-detect the appropriate JAVA_HOME value
#   based on whether the JDK or only the JRE is installed
RUN { echo '#!/bin/sh' ;echo 'set -e' ;echo ;echo 'dirname "$(dirname "$(readlink -f "$(which javac || which java)")")"' ; } > /usr/local/bin/docker-java-home \
 && chmod +x /usr/local/bin/docker-java-home
#   do some fancy footwork to create a JAVA_HOME that's cross-architecture-safe
RUN ln -svT "/usr/lib/jvm/java-8-openjdk-$( dpkg --print-architecture ;)" /docker-java-home
ENV JAVA_HOME="/docker-java-home"
ENV JAVA_VERSION="8u141"
ENV JAVA_DEBIAN_VERSION="8u141-b15-1~deb9u1"
#   see https://bugs.debian.org/775775
#   and https://github.com/docker-library/java/issues/19#issuecomment-70546872
ENV CA_CERTIFICATES_JAVA_VERSION="20170531+nmu1"
RUN set -ex ; if [ ! -d /usr/share/man/man1 ] ; then mkdir -p /usr/share/man/man1 ; fi ; apt-get update ; apt-get install --no-install-recommends jessie-backports openjdk-8-jdk ca-certificates-java=20190405 -y -t ; rm -rf /var/lib/apt/lists/* ; [ "$( readlink -f "$JAVA_HOME" ;)" = "$( docker-java-home ;)" ] ; update-alternatives --get-selections | awk -v home="$( readlink -f "$JAVA_HOME" ;)" 'index($3, home) == 1 { $2 = "manual"; print | "update-alternatives --set-selections" }' ; update-alternatives --query java | grep -q 'Status: manual'
#   see CA_CERTIFICATES_JAVA_VERSION notes above
RUN /var/lib/dpkg/info/ca-certificates-java.postinst configure
#  # done java
WORKDIR ${RASA_NLU_HOME}
COPY . ${RASA_NLU_HOME}
RUN pip install -r alt_requirements/requirements_mitie.txt
RUN pip install -e .
RUN apt-get update -qq \
 && apt-get install --no-install-recommends wget=1.20.1-1.1 -y \
 && wget -P data/ https://s3-eu-west-1.amazonaws.com/mitie/total_word_feature_extractor.dat \
 && apt-get remove -y wget \
 && apt-get autoremove -y
COPY sample_configs/config_mitie.json ${RASA_NLU_HOME}/config.json
VOLUME ["/app/projects", "/app/logs", "/app/data"]
EXPOSE 5000/tcp
ENTRYPOINT ["./entrypoint.sh"]
CMD ["start", "-c", "config.json"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
