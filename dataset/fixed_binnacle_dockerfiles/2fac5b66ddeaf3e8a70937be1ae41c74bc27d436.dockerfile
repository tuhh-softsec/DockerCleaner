#
#   Docker file that builds RelEx and starts the RelEx server.
#
#   To build:
#      docker build -t relex/relex .
#
#   To start:
#      docker run -p 3333:3333 relex/relex /bin/sh plain-text-server.sh
#
#   Or alternately, this:
#      docker run -p 4444:4444 relex/relex /bin/sh opencog-server.sh
#
#      docker run -p 9000:9000 relex/relex /bin/sh link-grammar-server.sh
#
#   To demo:
#      telnet localhost 4444
#      This is a test sentence!
#
#   That is, after connecting by telnet, type in any sentence, ending
#   with a period, and hit enter.  The response returned will be the
#   parse of the sentence, in opencog scheme format.
#
FROM ubuntu:18.04
MAINTAINER David Hart "dhart@opencog.org"
MAINTAINER Linas Vepštas "linasvepstas@gmail.com"
#   Avoid triggering apt-get dialogs (which may lead to errors). See:
#   http://stackoverflow.com/questions/25019183/docker-java7-install-fail
ENV DEBIAN_FRONTEND="noninteractive"
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"
#   Change line below only if you really, really need a newer OS version.
#   Otherwise, leave it alone, and the cache will be used.
ENV LAST_OS_UPDATE="2015-02-25"
RUN :
RUN apt-get -y upgrade
RUN (apt-get update ;apt-get install --no-install-recommends screen=4.6.2-1ubuntu1.1 telnet=0.17-41 netcat-openbsd=1.187-1ubuntu0.1 byobu=5.125-0ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.19.4-1ubuntu2.2 vim=2:8.0.1453-1ubuntu1.11 unzip=6.0-21ubuntu1.2 -y )
#   GCC and basic build tools
RUN (apt-get update ;apt-get install --no-install-recommends gcc=4:7.4.0-1ubuntu2.3 g++=4:7.4.0-1ubuntu2.3 make=4.1-9.1ubuntu1 -y )
#   Java
RUN (apt-get update ;apt-get install --no-install-recommends openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends ant=1.10.5-3~18.04 -y )
RUN (apt-get update ;apt-get install --no-install-recommends maven=3.6.0-1~18.04.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libcommons-logging-java=1.2-2 -y )
#   Wordnet
RUN (apt-get update ;apt-get install --no-install-recommends wordnet=1:3.0-35 -y )
RUN (apt-get update ;apt-get install --no-install-recommends wordnet-dev=1:3.0-35 -y )
#   RUN apt-get -y install wordnet-sense-index
#   There are UTF8 chars in the Java sources, and the RelEx build will
#   break if build in a C environment.
RUN (apt-get update ;apt-get install --no-install-recommends locales=2.27-3ubuntu1.6 -y )
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
RUN mkdir /usr/local/share/java
WORKDIR /home/Downloads/
#   Change line below on rebuild. Will use cache up to this line.
ENV LAST_SOFTWARE_UPDATE="2015-02-25"
#   Link Parser -- changes often
#   Download the current released version of link-grammar.
#   The wget gets the latest version w/ wildcard
RUN wget -r --no-parent -nH --cut-dirs=2 http://www.abisource.com/downloads/link-grammar/current/
#   Unpack the sources, too.
RUN tar -zxf current/link-grammar-5*.tar.gz
RUN (cd link-grammar-5.*/ ;./configure ;make -j6 ;make install ;ldconfig )
RUN LINKGRAMMAR_JAR=`find ./link-grammar* -name linkgrammar*.jar ` \
 && LINKGRAMMAR_VERSION=`echo $LINKGRAMMAR_JAR | grep -oP '(?<=-)\\d+\\.\\d+\\.\\d+(?=\\.)' ` \
 && mvn install:install-file -Dfile=$LINKGRAMMAR_JAR -DgroupId=org.opencog -DartifactId=linkgrammar -Dversion=$LINKGRAMMAR_VERSION -Dpackaging=jar
#   Relex -- changes often
RUN which wget &> /dev/null || (apt-get update ;apt-get install --no-install-recommends wget=1.20.3 ) ; wget --no-verbose --output-document /home/Downloads/relex-master.zip http://github.com/opencog/relex/archive/master.zip
RUN (unzip relex-master.zip ;cd relex-master ;mvn package )
#   Create and switch user. The user is privileged, with no password
#   required.  That is, you can use sudo.
RUN adduser --disabled-password --gecos "ReLex USER" relex
RUN adduser relex sudo
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
#   Punch out ports
#  # plain-text-server.sh port
EXPOSE 3333/tcp
#  # opencog-server.sh port
EXPOSE 4444/tcp
#  # link-grammar-server.sh port
EXPOSE 9000/tcp
WORKDIR /home/Downloads/relex-master/
USER relex
#   ENTRYPOINT bash -l -c ./opencog-server.sh
CMD /bin/bash
# Please add your HEALTHCHECK here!!!
