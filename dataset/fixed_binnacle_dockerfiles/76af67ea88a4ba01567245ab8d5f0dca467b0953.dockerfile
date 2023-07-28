FROM ubuntu:14.04
MAINTAINER Marvin Keller <marv@ramv.de>
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf=2.69-6 binutils-dev=2.24-5ubuntu14.2 bison=2:3.0.2.dfsg-2 build-essential=11.6ubuntu6 bzip2=1.0.6-5 ccache=3.1.9-1 cmake=2.8.12.2-0ubuntu3 curl=7.35.0-1ubuntu2.20 doxygen=1.8.6-2 flex=2.5.35-10.1ubuntu2 gcc-multilib=4:4.8.2-1ubuntu6 gdb=7.7.1-0ubuntu5~14.04.3 gfortran=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 git-flow=1.6.1-0ubuntu1 htop=1.0.2-3 libboost-all-dev=1.54.0.1ubuntu1 libev-dev=1:4.15-3 libgoogle-perftools-dev=2.1-2ubuntu1.1 libhwloc-dev=1.8-1ubuntu1.14.04.1 liblog4cxx10=0.10.0-1.2ubuntu3 liblog4cxx10-dev=0.10.0-1.2ubuntu3 libmetis-dev=5.1.0.dfsg-2 libmysqlclient-dev=5.5.62-0ubuntu0.14.04.1 libpapi-dev=5.3.0-3 libtbb-dev=4.2~20130725-1.1ubuntu1 libtool=2.4.2-1.7ubuntu1 libunwind8-dev=1.1-2.2ubuntu3 man mysql-server=5.5.62-0ubuntu0.14.04.1 nano=2.2.6-1ubuntu1 nodejs-legacy=0.10.25~dfsg2-2ubuntu1.2 npm=1.3.10~dfsg-1 openssh-server=1:6.6p1-2ubuntu2.13 python=2.7.5-5ubuntu3 python-pip=1.5.4-1ubuntu4 screen=4.1.0~20120320gitdb59704-9 strace=4.8-1ubuntu5 sudo=1.8.9p5-1ubuntu1.4 tcpdump=4.9.2-0ubuntu0.14.04.1 telnet=0.17-36build2 vim=2:7.4.052-1ubuntu3.1 wget=1.15-1ubuntu1.14.04.5 -y
RUN pip install sphinx==6.1.3 virtualenv==20.21.0 sphinxcontrib-seqdiag==3.0.0 webcolors==1.13 funcparserlib==1.0.1
#   HTTP Server for Web front ends
RUN npm install http-server@14.1.1 -g
ENV tmpDir="/tmp/hyrise"
RUN mkdir $tmpDir
WORKDIR $tmpDir
RUN wget http://downloads.sourceforge.net/project/libcsv/libcsv/libcsv-3.0.1/libcsv-3.0.1.tar.gz ; tar -xf libcsv-3.0.1.tar.gz ; cd $tmpDir/libcsv-3.0.1 ; make install -j 4
RUN cd $tmpdir ; git clone https://github.com/nanomsg/nanomsg.git ; cd nanomsg ; ./autogen.sh ; ./configure ; make ; make check ; make install ; rm -rf /tmp/nanomsg ; ldconfig
WORKDIR /tmp
RUN rm -rf $tmpDir
RUN mkdir /var/run/sshd
RUN /usr/bin/ssh-keygen -A
#   Set up my user
RUN useradd dev -u 1000 -s /bin/bash -m
USER dev
#  scm_breeze
RUN git clone git://github.com/ndbroadbent/scm_breeze.git ~/.scm_breeze
RUN ~/.scm_breeze/install.sh
USER root
#   Remove default motd
RUN rm /etc/update-motd.d/*
RUN rm /var/run/motd.dynamic
COPY motd /etc/motd
RUN sed -i '/motd.dynamic/d' /etc/pam.d/sshd
RUN sed -i '/motd.dynamic/d' /etc/pam.d/login
RUN rm /etc/legal
RUN gpasswd -a dev sudo
RUN echo 'dev:dev123' | chpasswd
#   Use non-shared directory for persistency. (Virtualbox shared folder don't support fsync - Problem with boot2docker)
RUN echo "export HYRISE_PERSISTENCY_PATH=/home/dev/hyrise_persistency" >> /etc/profile
#  Fix OSX locale problem
RUN locale-gen en_US en_US.UTF-8
RUN dpkg-reconfigure locales
ENTRYPOINT /usr/sbin/sshd -D
VOLUME ["/home/dev/workspace"]
VOLUME ["/home/dev/.ssh"]
EXPOSE 22/tcp
EXPOSE 5000/tcp
EXPOSE 8888/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
