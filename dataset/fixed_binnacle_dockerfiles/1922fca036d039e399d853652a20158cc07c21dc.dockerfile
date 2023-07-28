#   sshd
#
#   VERSION               0.0.2
FROM ubuntu:14.04
MAINTAINER InteractiveShell Team <trym2@googlegroups.com>
#   For ssh server and up-to-date ubuntu.
RUN apt-get update \
 && apt-get install --no-install-recommends openssh-server=1:6.6p1-2ubuntu2.13 wget=1.15-1ubuntu1.14.04.5 -y ; apt-get upgrade -y
RUN mkdir /build
#   Prerequisites
#
#   polymake
RUN apt-get install --no-install-recommends g++=4:4.8.2-1ubuntu6 libboost-dev=1.54.0.1ubuntu1 libgmp-dev=2:5.1.3+dfsg-1ubuntu1 libgmpxx4ldbl=2:5.1.3+dfsg-1ubuntu1 libmpfr-dev=3.1.2-1 libperl-dev=5.18.2-2ubuntu1.7 libsvn-perl=1.8.8-1ubuntu3.3 libterm-readline-gnu-perl=1.20-2build2 libxml-libxml-perl=2.0108+dfsg-1ubuntu0.2 libxml-libxslt-perl=1.84-1 libxml-perl=0.08-2 libxml-writer-perl=0.623-1 libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 w3c-dtd-xhtml=1.2-4 xsltproc=1.1.28-2ubuntu0.2 clang=1:3.4-0ubuntu1 bliss=0.72-5 libbliss-dev=0.72-5 make=3.81-8.2ubuntu3 -y
#   Singular
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 build-essential=11.6ubuntu6 autoconf=2.69-6 autogen=1:5.18-2ubuntu2 libtool=2.4.2-1.7ubuntu1 libreadline6-dev=6.3-4ubuntu2 libglpk-dev=4.52.1-2build1 libgmp-dev=2:5.1.3+dfsg-1ubuntu1 libmpfr-dev=3.1.2-1 libcdd-dev=094g-5 libntl-dev=5.5.2-2 -y
#   normaliz
RUN apt-get install --no-install-recommends zip=3.0-8 -y
#   Installing 4ti2
WORKDIR /build
ENV FOURTITWO_VERSION="1.6.6"
RUN wget http://www.4ti2.de/version_$FOURTITWO_VERSION/4ti2-$FOURTITWO_VERSION.tar.gz ; tar xzf 4ti2-$FOURTITWO_VERSION.tar.gz
WORKDIR /build/4ti2-$FOURTITWO_VERSION
RUN ./configure ; make -j3 ; make install
#   Installing normaliz
WORKDIR /build
ENV NORMALIZ_VERSION="2.12.2"
ENV NORMALIZ_VERSION_SHORT="2.12"
RUN wget http://www.home.uni-osnabrueck.de/wbruns/normaliz/Normaliz${NORMALIZ_VERSION}/Normaliz${NORMALIZ_VERSION}.zip ; unzip Normaliz${NORMALIZ_VERSION}.zip
WORKDIR /build/Normaliz${NORMALIZ_VERSION_SHORT}/source
RUN make -j3 ; cp /build/Normaliz${NORMALIZ_VERSION_SHORT}/source/normaliz /usr/local/bin
#   Installing polymake
WORKDIR /build
ENV POLYMAKE_VERSION="2.14"
RUN wget http://www.polymake.org/lib/exe/fetch.php/download/polymake-$POLYMAKE_VERSION.tar.bz2 ; tar jxf polymake-$POLYMAKE_VERSION.tar.bz2
WORKDIR /build/polymake-$POLYMAKE_VERSION
RUN ./configure CC=clang CXX=clang++ --without-java --without-javaview ; make -j2 ; make install
#   Installing Singular
WORKDIR /build
ENV LD_LIBRARY_PATH="$LIBRARY_PATH:/usr/local/lib"
RUN git clone https://github.com/Singular/Sources.git singular_sources
WORKDIR /build/singular_sources
RUN export PATH=$PATH:/usr/local/lib ; ./autogen.sh ; ./configure CXXFLAGS=-fopenmp --enable-gfanlib --enable-polymake ; make -j7 ; make install ; Singular -v
RUN ldconfig
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
RUN echo "UseDNS no" >> /etc/ssh/sshd_config
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
