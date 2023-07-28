#   sshd
#
#   VERSION               0.0.2
FROM ubuntu:16.04
MAINTAINER InteractiveShell Team <trym2@googlegroups.com>
#   For ssh server and up-to-date ubuntu.
RUN apt-get update \
 && apt-get install --no-install-recommends openssh-server=1:7.2p2-4ubuntu2.10 wget=1.17.1-1ubuntu1.5 -y ; apt-get upgrade -y
RUN mkdir /build
#   Prerequisites
#
#   polymake
RUN apt-get install --no-install-recommends g++=4:5.3.1-1ubuntu1 libboost-dev=1.58.0.1ubuntu1 libgmp-dev=2:6.1.0+dfsg-2 libgmpxx4ldbl=2:6.1.0+dfsg-2 libmpfr-dev=3.1.4-1 libperl-dev=5.22.1-9ubuntu0.9 libsvn-perl=1.9.3-2ubuntu1.3 libterm-readline-gnu-perl=1.28-2build1 libxml-libxml-perl=2.0123+dfsg-1ubuntu0.1 libxml-libxslt-perl=1.94-2build1 libxml-perl=0.08-2 libxml-writer-perl=0.625-1 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 xsltproc=1.1.28-2.1ubuntu0.3 clang=1:3.8-33ubuntu3.1 bliss=0.72-5 libbliss-dev=0.72-5 make=4.1-6 -y
#   Singular
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 build-essential=12.1ubuntu2 autoconf=2.69-9 autogen=1:5.18.7-3 libtool=2.4.6-0.1 libreadline6-dev=6.3-8ubuntu2 libglpk-dev=4.57-1build3 libgmp-dev=2:6.1.0+dfsg-2 libmpfr-dev=3.1.4-1 libcdd-dev=094g-5 libntl-dev=6.2.1-1 -y
#   normaliz
RUN apt-get install --no-install-recommends zip=3.0-11 -y
#   Installing 4ti2
WORKDIR /build
ENV FOURTITWO_VERSION="1.6.7"
RUN wget http://www.4ti2.de/version_$FOURTITWO_VERSION/4ti2-$FOURTITWO_VERSION.tar.gz ; tar xzf 4ti2-$FOURTITWO_VERSION.tar.gz
WORKDIR /build/4ti2-$FOURTITWO_VERSION
RUN ./configure ; make -j3 ; make install
#   Installing normaliz
WORKDIR /build
ENV NORMALIZ_VERSION="3.2.0"
RUN wget https://www.normaliz.uni-osnabrueck.de/wp-content/uploads/2017/01/normaliz-${NORMALIZ_VERSION}source.zip ; unzip normaliz-${NORMALIZ_VERSION}source.zip
WORKDIR /build/normaliz-${NORMALIZ_VERSION}/
RUN ./configure ; make -j3 ; make install
#   Installing polymake
WORKDIR /build
ENV POLYMAKE_VERSION="3.0"
ENV POLYMAKE_REVISION="r2"
RUN wget http://www.polymake.org/lib/exe/fetch.php/download/polymake-${POLYMAKE_VERSION}${POLYMAKE_REVISION}.tar.bz2 ; tar jxf polymake-${POLYMAKE_VERSION}${POLYMAKE_REVISION}.tar.bz2
WORKDIR /build/polymake-$POLYMAKE_VERSION
RUN ./configure CC=clang CXX=clang++ --without-java --without-javaview --without-atint ; make -j2 ; make install
#   Installing Singular
WORKDIR /build
ENV LD_LIBRARY_PATH="$LIBRARY_PATH:/usr/local/lib"
RUN git clone https://github.com/Singular/Sources.git singular_sources
WORKDIR /build/singular_sources
RUN ./autogen.sh ; ./configure CXXFLAGS=-fopenmp --enable-gfanlib ; make -j7 ; make install
#      Singular -v
RUN echo "interactive-shell!xE:echo:!echo -n \">>SPECIAL_EVENT_START>>\"%H\"<<SPECIAL_EVENT_END<<\"" >> /usr/local/share/singular/LIB/help.cnf
COPY surf.lib /usr/local/share/singular/LIB/surf.lib
RUN apt-get install --no-install-recommends surf-alggeo=1.0.6+ds-2 -y
#   Userland
WORKDIR /
RUN useradd -m -d /home/singularUser singularUser
RUN mkdir /home/singularUser/.ssh
COPY id_rsa.pub /home/singularUser/.ssh/authorized_keys
RUN chown -R singularUser:singularUser /home/singularUser/.ssh
RUN chmod 755 /home/singularUser/.ssh
RUN chmod 644 /home/singularUser/.ssh/authorized_keys
RUN mkdir /var/run/sshd
#   RUN echo 'root:screencast' | chpasswd
RUN sed -i 's/PermitRootLogin without-password/PermitRootLogin no/' /etc/ssh/sshd_config
#   SSH login fix. Otherwise user is kicked off after login
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd
ENV NOTVISIBLE="\"in users profile\""
RUN echo "export VISIBLE=now" >> /etc/profile
#   copy open
#   COPY open /usr/bin/open
#   RUN ln -s /usr/bin/open /usr/bin/display
WORKDIR /home/singularUser
#   USER singularUser
EXPOSE 22/tcp
#   CMD ["/usr/sbin/sshd", "-D"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
