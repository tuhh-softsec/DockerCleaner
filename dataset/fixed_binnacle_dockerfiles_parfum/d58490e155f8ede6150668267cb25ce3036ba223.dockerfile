FROM ubuntu:14.04
#  Set version and github repo which you want to build from
ENV GITHUB_OWNER="druid-io"
ENV DRUID_VERSION="master"
ENV ZOOKEEPER_VERSION="3.4.12"
#  Java 8
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common -y \
 && apt-add-repository -y ppa:webupd8team/java \
 && apt-get purge --auto-remove -y software-properties-common \
 && apt-get update \
 && echo oracle-java-8-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections \
 && apt-get install --no-install-recommends oracle-java8-installer oracle-java8-set-default mysql-server supervisor git python-pip -y \
 && apt-get clean \
 && rm -rf /var/cache/oracle-jdk8-installer \
 && rm -rf /var/lib/apt/lists/*
#  Maven
RUN wget -q -O - http://archive.apache.org/dist/maven/maven-3/3.3.9/binaries/apache-maven-3.3.9-bin.tar.gz | tar -xzf - -C /usr/local \
 && ln -s /usr/local/apache-maven-3.3.9 /usr/local/apache-maven \
 && ln -s /usr/local/apache-maven/bin/mvn /usr/local/bin/mvn
#  Zookeeper
RUN wget -q -O - http://www.us.apache.org/dist/zookeeper/zookeeper-$ZOOKEEPER_VERSION/zookeeper-$ZOOKEEPER_VERSION.tar.gz | tar -xzf - -C /usr/local \
 && cp /usr/local/zookeeper-$ZOOKEEPER_VERSION/conf/zoo_sample.cfg /usr/local/zookeeper-$ZOOKEEPER_VERSION/conf/zoo.cfg \
 && ln -s /usr/local/zookeeper-$ZOOKEEPER_VERSION /usr/local/zookeeper
#  Druid system user
RUN adduser --system --group --no-create-home druid \
 && mkdir -p /var/lib/druid \
 && chown druid:druid /var/lib/druid
#  Druid (from source)
RUN mkdir -p /usr/local/druid/lib
#  trigger rebuild only if branch changed
COPY https://api.github.com/repos/$GITHUB_OWNER/druid/git/refs/heads/$DRUID_VERSION druid-version.json
RUN git clone -q --branch $DRUID_VERSION --depth 1 https://github.com/$GITHUB_OWNER/druid.git /tmp/druid
WORKDIR /tmp/druid
#  package and install Druid locally
#  use versions-maven-plugin 2.1 to work around https://jira.codehaus.org/browse/MVERSIONS-285
RUN mvn -U -B org.codehaus.mojo:versions-maven-plugin:2.1:set -DgenerateBackupPoms=false -DnewVersion=$DRUID_VERSION \
 && mvn -U -B install -DskipTests=true -Dmaven.javadoc.skip=true \
 && cp services/target/druid-services-$DRUID_VERSION-selfcontained.jar /usr/local/druid/lib \
 && cp -r distribution/target/extensions /usr/local/druid/ \
 && cp -r distribution/target/hadoop-dependencies /usr/local/druid/ \
 && rm -rf /tmp/* /var/tmp/* /usr/local/apache-maven-3.3.9 /usr/local/apache-maven /root/.m2
WORKDIR /usr/local/druid
RUN java -cp "lib/*" -Ddruid.extensions.directory="extensions" -Ddruid.extensions.hadoopDependenciesDir="hadoop-dependencies" io.druid.cli.Main tools pull-deps --no-default-hadoop -c "org.apache.parquet:parquet-avro:1.9.0"
WORKDIR /
#  Setup metadata store
RUN /etc/init.d/mysql start \
 && mysql -u root -e "GRANT ALL ON druid.* TO 'druid'@'localhost' IDENTIFIED BY 'diurd'; CREATE database druid CHARACTER SET utf8;" \
 && java -cp /usr/local/druid/lib/druid-services-*-selfcontained.jar -Ddruid.extensions.directory=/usr/local/druid/extensions -Ddruid.extensions.loadList=["mysql-metadata-storage","druid-kafka-indexing-service"] -Ddruid.metadata.storage.type=mysql io.druid.cli.Main tools metadata-init --connectURI="jdbc:mysql://localhost:3306/druid" --user=druid --password=diurd \
 && /etc/init.d/mysql stop
#  Setup supervisord
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#  Expose ports:
#  - 8081: HTTP (coordinator)
#  - 8082: HTTP (broker)
#  - 8083: HTTP (historical)
#  - 8090: HTTP (overlord)
#  - 3306: MySQL
#  - 2181 2888 3888: ZooKeeper
EXPOSE 8081/tcp
EXPOSE 8082/tcp
EXPOSE 8083/tcp
EXPOSE 8090/tcp
EXPOSE 3306/tcp
EXPOSE 2181/tcp 2888/tcp 3888/tcp
WORKDIR /var/lib/druid
CMD export HOSTIP="$( resolveip -s $HOSTNAME ;)" \
 && exec /usr/bin/supervisord -c /etc/supervisor/conf.d/supervisord.conf
