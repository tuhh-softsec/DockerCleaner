FROM ubuntu:trusty
MAINTAINER Docker Hub
CMD ["sh", "adduser", "jenkins", "sudo"]
#   Set some variables	
ARG MAVEN_VERSION=3.3.9
ARG USER_HOME_DIR="/root"
ARG JAVA_VERSION=8
ARG PG_MAJOR=9.3
#   Add locales after locale-gen as needed
#   Upgrade packages on image
#   Preparations for sshd
RUN locale-gen en_US.UTF-8 \
 && : \
 && DEBIAN_FRONTEND="noninteractive" apt-get -q upgrade -y -o Dpkg::Options::="--force-confnew" --no-install-recommends \
 && DEBIAN_FRONTEND="noninteractive" apt-get -q install -y -o Dpkg::Options::="--force-confnew" --no-install-recommends openssh-server \
 && apt-get -q autoremove \
 && apt-get -q clean -y \
 && rm -rf /var/lib/apt/lists/* \
 && rm -f /var/cache/apt/*.bin \
 && sed -i 's|session required pam_loginuid.so|session optional pam_loginuid.so|g' /etc/pam.d/sshd \
 && mkdir -p /var/run/sshd
#   Sets language to UTF8 : this works in pretty much all cases	
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
#   Install JDK
#   Install the python script required for "add-apt-repository"
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.92.37.8 -y )
#   Setup the OpenJDK repo
RUN add-apt-repository ppa:openjdk-r/ppa
#   Installing ...
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends openjdk-$JAVA_VERSION-jdk -y )
#   Setup JAVA_HOME, this is useful for docker commandline
ENV JAVA_HOME="/usr/lib/jvm/java-$JAVA_VERSION-openjdk-amd64/"
RUN export JAVA_HOME
#   Set user jenkins to the image
RUN useradd -m -d /home/jenkins -s /bin/sh jenkins \
 && echo "jenkins:jenkins" | chpasswd
RUN adduser jenkins sudo
#   Install curl
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends curl=7.35.0-1ubuntu2.20 git=1:1.9.1-1ubuntu0.10 -y ) \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists
#   Install Maven
RUN mkdir -p /usr/share/maven /usr/share/maven/ref \
 && curl -fsSL http://apache.osuosl.org/maven/maven-3/$MAVEN_VERSION/binaries/apache-maven-$MAVEN_VERSION-bin.tar.gz | tar -xzC /usr/share/maven --strip-components=1 \
 && ln -s /usr/share/maven/bin/mvn /usr/bin/mvn
ENV MAVEN_HOME="/usr/share/maven"
ENV MAVEN_CONFIG="\"$USER_HOME_DIR/.m2\""
VOLUME "$USER_HOME_DIR/.m2"
#   SSH port
EXPOSE 22/tcp
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libpostgresql-jdbc-java=9.2-1002-1 )
RUN groupadd -r postgres --gid=999 \
 && useradd -r -g postgres --uid=999 postgres
#   make the "en_US.UTF-8" locale so postgres will be utf-8 enabled by default
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends locales=2.13+git20120306-12.1 -y ) \
 && rm -rf /var/lib/apt/lists/* \
 && localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8
ENV LANG="en_US.utf8"
#   Install postgresql
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ trusty-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
 && : \
 && DEBIAN_FRONTEND=noninteractive apt-get install -y --force-yes postgresql-$PG_MAJOR postgresql-client-$PG_MAJOR postgresql-contrib-$PG_MAJOR \
 && /etc/init.d/postgresql stop
#   make the sample config easier to munge (and "correct by default")
RUN mv -v /usr/share/postgresql/$PG_MAJOR/postgresql.conf.sample /usr/share/postgresql/ \
 && ln -sv ../postgresql.conf.sample /usr/share/postgresql/$PG_MAJOR/ \
 && sed -ri "s!^#?(listen_addresses)\s*=\s*\S+.*!\1 = '*'!" /usr/share/postgresql/postgresql.conf.sample
RUN mkdir -p /var/run/postgresql \
 && chown -R postgres:postgres /var/run/postgresql \
 && chmod g+s /var/run/postgresql
ENV PATH="/usr/lib/postgresql/$PG_MAJOR/bin:$PATH"
ENV PGDATA="/var/lib/postgresql/data"
RUN mkdir -p "$PGDATA" \
 && chown -R postgres:postgres "$PGDATA" \
 && chmod 777 "$PGDATA"
VOLUME /var/lib/postgresql/data
RUN sudo /etc/init.d/postgresql start \
 && sudo -u postgres psql -c "ALTER USER postgres PASSWORD 'postgres';" \
 && sudo -u postgres psql -c "CREATE USER oak WITH SUPERUSER PASSWORD 'oakpwd';" \
 && sudo -u postgres createdb -O oak oakdb
ENV PAYARA_PKG="https://s3-eu-west-1.amazonaws.com/payara.fish/Payara+Downloads/Payara+4.1.1.164/payara-4.1.1.164.zip"
ENV PAYARA_VERSION="164"
ENV PKG_FILE_NAME="payara-full-$PAYARA_VERSION.zip"
ENV PAYARA_PATH="/opt/payara41"
ENV ADMIN_USER="admin"
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends wget=1.15-1ubuntu1.14.04.5 -y ) \
 && rm -rf /var/lib/apt/lists/*
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends unzip=6.0-9ubuntu1.5 -y )
RUN wget --quiet -O /opt/$PKG_FILE_NAME $PAYARA_PKG
RUN unzip -qq /opt/$PKG_FILE_NAME -d /opt
RUN mkdir -p $PAYARA_PATH/deployments
RUN useradd -b /opt -m -s /bin/bash -d $PAYARA_PATH payara \
 && echo payara:payara | chpasswd
RUN chown -R payara:payara /opt
#   Default payara ports to expose
EXPOSE 4848/tcp 8009/tcp 8080/tcp 8181/tcp
RUN : \
 && apt-get upgrade -y \
 && cd /opt \
 && wget -c http://apache.mirror.anlx.net//jmeter/binaries/apache-jmeter-3.1.tgz \
 && tar xzf apache-jmeter-3.1.tgz \
 && rm apache-jmeter-3.1.tgz \
 && mv apache-jmeter-3.1 jmeter \
 && ln -s /opt/jmeter/bin/jmeter /usr/bin/jmeter \
 && rm -rf /var/lib/apt/lists/*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
