#   This is a comment
FROM ubuntu:14.04
MAINTAINER René Eigenheer <reigenheer@bridgesolutions.net>
RUN apt-get update \
 && apt-get install --no-install-recommends apache2=2.4.7-1ubuntu4.22 curl=7.35.0-1ubuntu2.20 libapache2-svn=1.8.8-1ubuntu3.3 openssh-server=1:6.6p1-2ubuntu2.13 python-software-properties=0.92.37.8 software-properties-common=0.92.37.8 subversion=1.8.8-1ubuntu3.3 -y
#  ## Jenkins, Oracle Java 7
RUN wget -q -O - https://jenkins-ci.org/debian/jenkins-ci.org.key | sudo apt-key add - ; sh -c 'echo deb http://pkg.jenkins-ci.org/debian binary/ > /etc/apt/sources.list.d/jenkins.list' ; add-apt-repository ppa:webupd8team/java -y ; apt-get update ; echo oracle-java7-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections ; apt-get install --no-install-recommends oracle-java7-installer jenkins maven=3.0.5-1 -y
#  ## get Jenkins Maven Config	
COPY data/jenkins/varLib /var/lib/jenkins
COPY data/maven/settings.xml /etc/maven/settings.xml
RUN wget -P /var/lib/jenkins/plugins http://updates.jenkins-ci.org/latest/parameterized-trigger.hpi ; wget -P /var/lib/jenkins/plugins http://updates.jenkins-ci.org/latest/extended-choice-parameter.hpi ; wget -P /var/lib/jenkins/plugins http://updates.jenkins-ci.org/latest/build-blocker-plugin.hpi ; wget -P /var/lib/jenkins/plugins http://updates.jenkins-ci.org/latest/console-column-plugin.hpi ; wget -P /var/lib/jenkins/plugins http://updates.jenkins-ci.org/latest/greenballs.hpi ; chown -R jenkins:jenkins /var/lib/jenkins
#  ## SSH
RUN sed -i 's/PermitRootLogin without-password/PermitRootLogin yes/' /etc/ssh/sshd_config ; mkdir -p /var/run/sshd
#  ## Apache
COPY data/apache /etc/apache2/sites-available
RUN a2dissite 000-default ; a2ensite repo ; a2ensite ci ; a2ensite p2 ; a2ensite svn ; a2enmod proxy ; a2enmod proxy_http ; a2enmod rewrite
#  	a2enmod ssl
#  ## Subversion
COPY data/import /data/import
RUN printf "\n127.0.0.1\trepo.sbb.ch\n127.0.0.1\tci.sbb.ch\n127.0.0.1\tsvn.sbb.ch\n185.31.17.209\trepo1.maven.org\n127.0.0.1\tp2.sbb.ch\n198.41.30.249\tmaven.eclipse.org\n" >> /etc/hosts; mkdir -p /data/svn ; svnadmin create /data/svn/cisi ; chown -R www-data:www-data /data/svn ; cd /data/import ; mkdir /data/import/svn ; tar -xvzf svnsbbcisi.tar.gz -C svn ; /etc/init.d/apache2 start ; svn import /data/import/svn http://svn.sbb.ch/svn/cisi/trunk -m import
#  ## Nexus
RUN adduser --disabled-password --disabled-login --gecos "" nexus ; wget -P /usr/local/ http://www.sonatype.org/downloads/nexus-latest-bundle.tar.gz ; cd /usr/local ; tar xvzf nexus-latest-bundle.tar.gz ; ln -s /usr/local/nexus-2* /usr/local/nexus
COPY data/sonatype-work /usr/local/sonatype-work
COPY data/nexus/conf/nexus.properties /usr/local/nexus/conf/nexus.properties
RUN chown -R nexus:nexus /usr/local/nexus-2* ; chown -R nexus:nexus /usr/local/sonatype-work/
RUN cd /data/import ; tar -xvzf p2gef.tar.gz -C /var/www ; chown -R www-data:www-data /var/www/p2.sbb.ch/
#  ## Jenkins Load
COPY data/jenkins/importTool /root/importTool
COPY prepareEnvironment.sh /root/prepareEnvironment.sh
RUN chmod 777 /root/prepareEnvironment.sh
EXPOSE 22/tcp 80/tcp 8080/tcp 8081/tcp
COPY startup.sh /root/startup.sh
RUN chmod 777 /root/startup.sh
#  ## root password
RUN echo 'root:cisi@docker' | chpasswd
CMD ["/bin/bash", "/root/startup.sh"]
#   TODO
#   - evtl p2.sbb.ch als redirect auf internet sites -> platzersparnis, nachteil nicht 100% SBB
#   - svn als https
#   - ci als https
#   - aufteilen mehere container
#   docker build --no-cache -t ere00/build:v1 .
#   docker build -t ere00/build:v1 .
#   docker run -d -p 80:80 -p 8080:8080 -p 8081:8081 -p 22222:22 --name test_build ere00/build:v1
#   ssh -p 22222 -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no root@192.168.59.103
#   ./importJenkins.sh
#   build pt.cisi.tools.eclipse.tycho.manual
#   build pt.cisi.3rdparty.target.continous
#   build pt.cisi.3rdparty.bundles.manual
#   build pt.cisi.3rdparty.eclipse.manual
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
