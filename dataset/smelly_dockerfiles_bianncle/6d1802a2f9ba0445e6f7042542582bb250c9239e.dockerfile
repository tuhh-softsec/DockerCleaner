FROM quay.io/openshift/origin-cli
#  Jenkins image for OpenShift
#
#  This image provides a Jenkins server, primarily intended for integration with
#  OpenShift v3.
#
#  Volumes: 
#  * /var/jenkins_home
#  Environment:
#  * $JENKINS_PASSWORD - Password for the Jenkins 'admin' user.
MAINTAINER Ben Parees <bparees@redhat.com>
ENV JENKINS_VERSION="2" \
    HOME="/var/lib/jenkins" \
    JENKINS_HOME="/var/lib/jenkins" \
    JENKINS_UC="https://updates.jenkins.io" \
    OPENSHIFT_JENKINS_IMAGE_VERSION="4.0" \
    LANG="en_US.UTF-8" \
    LC_ALL="en_US.UTF-8" \
    INSTALL_JENKINS_VIA_RPMS="false"
#  openshift/ocp-build-data will change INSTALL_JENKINS_VIA_RPMS to true
#  so that the osbs/brew builds will install via RPMs; when this runs 
#  in api.ci, it will employ the old centos style, download the plugins and
#  redhat-stable core RPM for download
LABEL io.k8s.description="Jenkins is a continuous integration server" \
      io.k8s.display-name="Jenkins 2" \
      io.openshift.tags="jenkins,jenkins2,ci" \
      io.openshift.expose-services="8080:http" \
      io.openshift.s2i.scripts-url="image:///usr/libexec/s2i"
#  Labels consumed by Red Hat build service
LABEL com.redhat.component="openshift-jenkins-2-container" \
      name="openshift3/jenkins-2-rhel7" \
      version="4.0" \
      architecture="x86_64"
#  8080 for main web interface, 50000 for slave agents
EXPOSE 8080/tcp 50000/tcp
RUN yum-config-manager --disable epel > /dev/null || : \
 && yum-config-manager --enable rhel-7-server-ose-onlineint-rpms || : \
 && ln -s /usr/lib/jenkins /usr/lib64/jenkins \
 && INSTALL_PKGS="dejavu-sans-fonts wget rsync gettext git tar zip unzip openssl bzip2 dumb-init java-1.8.0-openjdk java-1.8.0-openjdk-devel" \
 && yum install -y $INSTALL_PKGS \
 && rpm -V $INSTALL_PKGS \
 && yum clean all \
 && localedef -f UTF-8 -i en_US en_US.UTF-8
COPY ./contrib/openshift /opt/openshift
COPY ./contrib/jenkins /usr/local/bin
ADD ./contrib/s2i /usr/libexec/s2i
ADD release.version /tmp/release.version
RUN /usr/local/bin/install-jenkins-core-plugins.sh /opt/openshift/base-plugins.txt \
 && rmdir /var/log/jenkins \
 && chmod 664 /etc/passwd \
 && chmod -R 775 /etc/alternatives \
 && chmod -R 775 /var/lib/alternatives \
 && chmod -R 775 /usr/lib/jvm \
 && chmod 775 /usr/bin \
 && chmod 775 /usr/lib/jvm-exports \
 && chmod 775 /usr/share/man/man1 \
 && mkdir -p /var/lib/origin \
 && chmod 775 /var/lib/origin \
 && unlink /usr/bin/java \
 && unlink /usr/bin/jjs \
 && unlink /usr/bin/keytool \
 && unlink /usr/bin/orbd \
 && unlink /usr/bin/pack200 \
 && unlink /usr/bin/policytool \
 && unlink /usr/bin/rmid \
 && unlink /usr/bin/rmiregistry \
 && unlink /usr/bin/servertool \
 && unlink /usr/bin/tnameserv \
 && unlink /usr/bin/unpack200 \
 && unlink /usr/lib/jvm-exports/jre \
 && unlink /usr/share/man/man1/java.1.gz \
 && unlink /usr/share/man/man1/jjs.1.gz \
 && unlink /usr/share/man/man1/keytool.1.gz \
 && unlink /usr/share/man/man1/orbd.1.gz \
 && unlink /usr/share/man/man1/pack200.1.gz \
 && unlink /usr/share/man/man1/policytool.1.gz \
 && unlink /usr/share/man/man1/rmid.1.gz \
 && unlink /usr/share/man/man1/rmiregistry.1.gz \
 && unlink /usr/share/man/man1/servertool.1.gz \
 && unlink /usr/share/man/man1/tnameserv.1.gz \
 && unlink /usr/share/man/man1/unpack200.1.gz \
 && chown -R 1001:0 /opt/openshift \
 && /usr/local/bin/fix-permissions /opt/openshift \
 && /usr/local/bin/fix-permissions /opt/openshift/configuration/init.groovy.d \
 && /usr/local/bin/fix-permissions /var/lib/jenkins \
 && /usr/local/bin/fix-permissions /var/log
VOLUME ["/var/lib/jenkins"]
USER 1001
ENTRYPOINT ["/usr/bin/dumb-init", "--"]
CMD ["/usr/libexec/s2i/run"]
