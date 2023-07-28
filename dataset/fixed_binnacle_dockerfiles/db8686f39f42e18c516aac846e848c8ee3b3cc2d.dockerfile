#   Copyright 2016 Teradata
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
FROM teradatalabs/cdh5-hive:unlabelled
MAINTAINER Teradata Docker Team <docker@teradata.com>
#   INSTALL KERBEROS
RUN yum install -y krb5-libs krb5-server krb5-workstation
#   ADD KERBEROS CONFIGURATION
COPY files/kerberos/krb5.conf /etc/krb5.conf
COPY files/kerberos/kdc.conf /var/kerberos/krb5kdc/kdc.conf
COPY files/kerberos/kadm5.acl /var/kerberos/krb5kdc/kadm5.acl
#   CREATE KERBEROS DATABASE
RUN /usr/sbin/kdb5_util create -s -P password
#   ADD SUPERVISORD BOOTSTRAP KERBEROS SCRIPT
COPY files/bootstrap-kerberos.sh /root/bootstrap-kerberos.sh
COPY files/supervisord.d/bootstrap-kerberos.conf /etc/supervisord.d/bootstrap-kerberos.conf
#   ADD HADOOP PRINCIPALS
RUN /usr/sbin/kadmin.local -q "addprinc -randkey hdfs/hadoop-master@LABS.TERADATA.COM"
RUN /usr/sbin/kadmin.local -q "addprinc -randkey mapred/hadoop-master@LABS.TERADATA.COM"
RUN /usr/sbin/kadmin.local -q "addprinc -randkey yarn/hadoop-master@LABS.TERADATA.COM"
RUN /usr/sbin/kadmin.local -q "addprinc -randkey HTTP/hadoop-master@LABS.TERADATA.COM"
#   CREATE HADOOP KEYTAB FILES
RUN /usr/sbin/kadmin.local -q "xst -norandkey -k /etc/hadoop/conf/hdfs.keytab hdfs/hadoop-master HTTP/hadoop-master"
RUN /usr/sbin/kadmin.local -q "xst -norandkey -k /etc/hadoop/conf/mapred.keytab mapred/hadoop-master HTTP/hadoop-master"
RUN /usr/sbin/kadmin.local -q "xst -norandkey -k /etc/hadoop/conf/yarn.keytab yarn/hadoop-master HTTP/hadoop-master"
RUN /usr/sbin/kadmin.local -q "xst -norandkey -k /etc/hadoop/conf/HTTP.keytab HTTP/hadoop-master"
RUN chown hdfs:hadoop /etc/hadoop/conf/hdfs.keytab
RUN chown mapred:hadoop /etc/hadoop/conf/mapred.keytab
RUN chown yarn:hadoop /etc/hadoop/conf/yarn.keytab
RUN chown hdfs:hadoop /etc/hadoop/conf/HTTP.keytab
RUN chmod 644 /etc/hadoop/conf/*.keytab
#   ENABLE HADOOP SECURITY
COPY files/conf/core-site.xml /etc/hadoop/conf/core-site.xml
COPY files/conf/hdfs-site.xml /etc/hadoop/conf/hdfs-site.xml
#   ENABLE MAPRED SECURITY
COPY files/conf/mapred-site.xml /etc/hadoop/conf/mapred-site.xml
COPY files/conf/taskcontroller.cfg /etc/hadoop/conf/taskcontroller.cfg
#   ENABLE YARN SECURITY
COPY files/conf/yarn-site.xml /etc/hadoop/conf/yarn-site.xml
COPY files/conf/container-executor.cfg /etc/hadoop/conf/container-executor.cfg
RUN chmod 6050 /etc/hadoop/conf/container-executor.cfg
#   CREATE HIVE PRINCIPAL AND KEYTAB
RUN /usr/sbin/kadmin.local -q "addprinc -randkey hive/hadoop-master@LABS.TERADATA.COM"
RUN /usr/sbin/kadmin.local -q "xst -norandkey -k /etc/hive/conf/hive.keytab hive/hadoop-master"
#   ENABLE HIVE SECURITY
COPY files/conf/hive-site.xml /etc/hive/conf/hive-site.xml
RUN chown hive:hadoop /etc/hive/conf/hive.keytab
RUN chmod 644 /etc/hive/conf/hive.keytab
#   ENABLE AUTHORIZATION IN HIVE SERVER 
COPY files/conf/hiveserver2-site.xml /etc/hive/conf/hiveserver2-site.xml
#   CREATE PRESTO PRINCIPAL AND KEYTAB
RUN /usr/sbin/kadmin.local -q "addprinc -randkey presto-server/presto-master.docker.cluster@LABS.TERADATA.COM"
RUN /usr/sbin/kadmin.local -q "addprinc -randkey presto-server/presto-worker.docker.cluster@LABS.TERADATA.COM"
RUN /usr/sbin/kadmin.local -q "addprinc -randkey presto-server/presto-worker-1.docker.cluster@LABS.TERADATA.COM"
RUN /usr/sbin/kadmin.local -q "addprinc -randkey presto-server/presto-worker-2.docker.cluster@LABS.TERADATA.COM"
RUN /usr/sbin/kadmin.local -q "addprinc -randkey HTTP/presto-master.docker.cluster@LABS.TERADATA.COM"
RUN /usr/sbin/kadmin.local -q "addprinc -randkey presto-client/presto-master.docker.cluster@LABS.TERADATA.COM"
RUN /usr/sbin/kadmin.local -q "addprinc -randkey hive/presto-master.docker.cluster@LABS.TERADATA.COM"
RUN mkdir -p /etc/presto/conf
RUN /usr/sbin/kadmin.local -q "xst -norandkey -k /etc/presto/conf/presto-server.keytab presto-server/presto-master.docker.cluster presto-server/presto-worker.docker.cluster presto-server/presto-worker-1.docker.cluster presto-server/presto-worker-2.docker.cluster"
RUN /usr/sbin/kadmin.local -q "xst -norandkey -k /etc/presto/conf/presto-server-HTTP.keytab HTTP/presto-master.docker.cluster"
RUN /usr/sbin/kadmin.local -q "xst -norandkey -k /etc/presto/conf/presto-client.keytab presto-client/presto-master.docker.cluster"
RUN /usr/sbin/kadmin.local -q "xst -norandkey -k /etc/presto/conf/hive-presto-master.keytab hive/presto-master.docker.cluster"
RUN chmod 644 /etc/presto/conf/*.keytab
#   CREATE SSL KEYSTORE
RUN keytool -genkeypair -alias presto -keyalg RSA -keystore /etc/presto/conf/keystore.jks -keypass password -storepass password -dname "CN=presto-master, OU=, O=, L=, S=, C="
RUN chmod 644 /etc/presto/conf/keystore.jks
#   EXPOSE KERBEROS PORTS
EXPOSE 88/tcp
EXPOSE 749/tcp
CMD /root/startup.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
