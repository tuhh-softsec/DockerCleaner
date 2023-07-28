#   Base java image
#   Version 3.7 because on 3.8 Python could not find a version that satisfies the requirement cffi!=1.11.3,>=1.8
FROM openjdk:alpine3.7
#   Set environment
ENV NIFI_VERSION="1.7.0"
ENV GLIBC_VERSION="2.25-r0"
#   Install required packages
RUN apk add ca-certificates=20190108-r0 wget=1.20.3-r0 bash=4.4.19-r1 java-snappy=1.1.2.6-r1 --update --no-cache \
 && update-ca-certificates
#   Download nifi
RUN wget -qO- https://archive.apache.org/dist/nifi/$NIFI_VERSION/nifi-$NIFI_VERSION-bin.tar.gz | tar -xz \
 && mv nifi-$NIFI_VERSION /usr \
 && mv /usr/nifi-$NIFI_VERSION /usr/nifi
#   Install glibc
RUN wget --no-cache -q -O /etc/apk/keys/sgerrand.rsa.pub https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub \
 && wget -q https://github.com/sgerrand/alpine-pkg-glibc/releases/download/$GLIBC_VERSION/glibc-$GLIBC_VERSION.apk \
 && apk add glibc-$GLIBC_VERSION.apk \
 && rm glibc-$GLIBC_VERSION.apk
RUN rm -r /usr/nifi/conf
#   Link kylo jars
COPY dist/ /usr/nifi/kylo
RUN mkdir -p /usr/nifi/lib/app
RUN mkdir -p /usr/nifi/dump
#  ###############################################################################
#  ###############################################################################
#  #
#  #   WARNING !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!!
#  #
#  # When you upgrade to a new version of Kylo or Nifi please double check that
#  # you are still making symbolic links to the correct version ho the NARs (e.g.
#  # v1 vs v1.2)
#  #
#  #
RUN ln -f -s /usr/nifi/kylo/kylo-nifi-core-service-v1.2-nar-*.nar /usr/nifi/lib/kylo-nifi-core-service-nar.nar
RUN ln -f -s /usr/nifi/kylo/kylo-nifi-standard-services-v1.2-nar-*.nar /usr/nifi/lib/kylo-nifi-standard-services-nar.nar
RUN ln -f -s /usr/nifi/kylo/kylo-nifi-core-v1.2-nar-*.nar /usr/nifi/lib/kylo-nifi-core-nar.nar
RUN ln -f -s /usr/nifi/kylo/kylo-nifi-spark-v1.2-nar-*.nar /usr/nifi/lib/kylo-nifi-spark-nar.nar
RUN ln -f -s /usr/nifi/kylo/kylo-nifi-spark-service-v1.2-nar-*.nar /usr/nifi/lib/kylo-nifi-spark-service-nar.nar
RUN ln -f -s /usr/nifi/kylo/kylo-nifi-hadoop-v1.2-nar-*.nar /usr/nifi/lib/kylo-nifi-hadoop-nar.nar
RUN ln -f -s /usr/nifi/kylo/kylo-nifi-hadoop-service-v1.2-nar-*.nar /usr/nifi/lib/kylo-nifi-hadoop-service-nar.nar
RUN ln -f -s /usr/nifi/kylo/kylo-nifi-elasticsearch-v1-nar-*.nar /usr/nifi/lib/kylo-nifi-elasticsearch-nar.nar
RUN ln -f -s /usr/nifi/kylo/kylo-nifi-teradata-tdch-nar-*.nar /usr/nifi/lib/kylo-nifi-teradata-tdch-nar.nar
#   Create links for custom kylo provenance
RUN ln -f -s /usr/nifi/kylo/kylo-nifi-provenance-repo-v1.2-nar-*.nar /usr/nifi/lib/kylo-nifi-provenance-repo-nar.nar
#   Create links for custom kylo-spark jars
RUN ln -f -s /usr/nifi/kylo/kylo-spark-validate-cleanse-v2-*-jar-with-dependencies.jar /usr/nifi/lib/app/kylo-spark-validate-cleanse-jar-with-dependencies.jar
RUN ln -f -s /usr/nifi/kylo/kylo-spark-job-profiler-v2-*-jar-with-dependencies.jar /usr/nifi/lib/app/kylo-spark-job-profiler-jar-with-dependencies.jar
RUN ln -f -s /usr/nifi/kylo/kylo-spark-interpreter-v2-*-jar-with-dependencies.jar /usr/nifi/lib/app/kylo-spark-interpreter-jar-with-dependencies.jar
RUN ln -f -s /usr/nifi/kylo/kylo-spark-merge-table-v2-*-jar-with-dependencies.jar /usr/nifi/lib/app/kylo-spark-merge-table-jar-with-dependencies.jar
RUN ln -f -s /usr/nifi/kylo/kylo-spark-multi-exec-v2-*-jar-with-dependencies.jar /usr/nifi/lib/app/kylo-spark-multi-exec-jar-with-dependencies.jar
#  #
#  #
#  ###############################################################################
#  ###############################################################################
#   Create links for Teamdigitale custom processors
RUN ln -f -s /usr/nifi/kylo/daf/teamdigitale-daf-nifi-processors-*.nar /usr/nifi/lib/teamdigitale-daf-nifi-processors.nar
RUN mkdir -p /usr/nifi/activemq
RUN mkdir -p /usr/nifi/drivers
RUN mkdir -p /usr/nifi/drivers/impala
RUN mkdir -p /usr/nifi/conf.temp
RUN wget http://central.maven.org/maven2/org/apache/activemq/activemq-all/5.15.0/activemq-all-5.15.0.jar -P /usr/nifi/activemq/
RUN wget http://central.maven.org/maven2/org/mariadb/jdbc/mariadb-java-client/2.1.0/mariadb-java-client-2.1.0.jar -P /usr/nifi/drivers/
#  download Impala JDBC driver
RUN wget -q https://downloads.cloudera.com/connectors/impala_jdbc_2.5.42.1062.zip
RUN unzip impala_jdbc_2.5.42.1062.zip
RUN unzip ClouderaImpalaJDBC-2.5.42.1062/ClouderaImpalaJDBC4_2.5.42.zip
RUN mv ClouderaImpalaJDBC4_2.5.42/* /usr/nifi/drivers/impala
#   Expose default nifi port
EXPOSE 8080/tcp
RUN apk update
RUN apk add krb5-libs=1.15.4-r0 krb5=1.15.4-r0 krb5-dev=1.15.4-r0 sudo=1.8.21_p2-r1 coreutils=8.28-r0 python cyrus-sasl=2.1.26-r11 alpine-sdk=0.5-r0 libsasl=2.1.26-r11 libffi-dev=3.2.1-r4 openssl-dev=1.0.2t-r0 cyrus-sasl-dev=2.1.26-r11 --no-cache \
 && apk add python3=3.6.9-r1 python3-dev=3.6.9-r1 --no-cache \
 && python3 -m ensurepip \
 && rm -r /usr/lib/python*/ensurepip \
 && pip3 install --upgrade pip setuptools thrift_sasl==0.2.1 sasl impyla hdfs polling requests_kerberos
COPY scripts/ /scripts
RUN chmod a+x /scripts/*
#   Start nifi
CMD /scripts/setup.sh ; /usr/nifi/bin/nifi.sh run
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
