#   namin/io.livecode.ch
FROM ubuntu:18.04
MAINTAINER Nada Amin, namin@alum.mit.edu
RUN sed -i 's/# \(.*multiverse$\)/\1/g' /etc/apt/sources.list \
 && apt-get update \
 && apt-get -y upgrade \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=12.4ubuntu1 -y ) \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 -y )
RUN (apt-get update ;apt-get install --no-install-recommends locales=2.27-3ubuntu1.6 )
RUN locale-gen en_US en_US.UTF-8
ENV TZ="America/New_York"
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
 && echo $TZ > /etc/timezone
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 wget=1.19.4-1ubuntu2.2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.17.1-1ubuntu0.17 subversion=1.9.7-4ubuntu1.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends unzip=6.0-21ubuntu1.2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends sed=4.4-2 gawk=1:4.1.4+dfsg-1build1 -y )
RUN mkdir /code
#   NOTE(namin): I disabled some installations from source, because they get killed.
#  ## Scheme ###
#  ## Vicare ###
#
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=12.4ubuntu1 libtool=2.4.6-2 autoconf=2.69-11 libgmp-dev=2:6.1.2+dfsg-2ubuntu0.1 texinfo=6.5.0.dfsg.1-2 -y )
#   --- killed ---
#   RUN cd /code;\
#       git clone https://github.com/marcomaggi/vicare.git;\
#       cd vicare;\
#       sh autogen.sh;\
#       mkdir build;\
#       cd build;\
#       ../configure --enable-maintainer-mode;\
#       make;\
#      make install
#
#  ### Install from binary ####
RUN cd /code ; mkdir vicare ; cd vicare ; wget -nv http://lampwww.epfl.ch/~amin/dkr/vicare/vicare ; wget -nv http://lampwww.epfl.ch/~amin/dkr/vicare/vicare-lib.zip ; unzip vicare-lib.zip -d /usr/local/lib/ ; chmod 755 vicare ; cp vicare /usr/local/bin/
#  ## Chez ###
RUN (apt-get update ;apt-get install --no-install-recommends libncurses-dev ncurses-dev libx11-dev=2:1.6.4-3ubuntu0.4 -y )
#   RUN cd /code;\
#      wget -nv http://github.com/cisco/ChezScheme/archive/v9.4.zip;\
#      unzip v9.4.zip -d .;\
#      cd ChezScheme-9.4;\
#      ./configure;\
#      make install
#  ### Install from binary ####
#  RUN cd /usr/local/bin;\
#      wget -nv http://lampwww.epfl.ch/~amin/dkr/chez/scheme;\
#      chmod 755 scheme;\
#      cd /usr/local/lib;\
#      mkdir -p csv9.4/a6le;\
#      cd csv9.4/a6le;\
#      wget -nv http://lampwww.epfl.ch/~amin/dkr/chez/petite.boot;\
#      wget -nv http://lampwww.epfl.ch/~amin/dkr/chez/scheme.boot
RUN (apt-get update ;apt-get install --no-install-recommends chezscheme=9.5+dfsg-2build2 -y )
#  ## ML ###
RUN (apt-get update ;apt-get install --no-install-recommends mlton-compiler=20130715-3 -y )
#  ## Twelf ###
#   --- killed ---
#   RUN cd /code;\
#       git clone https://github.com/standardml/twelf.git;\
#       cd twelf;\
#       make mlton;\
#       make install
#
#  ### Install from binary ####
RUN cd /code ; mkdir twelf ; wget -nv http://lampwww.epfl.ch/~amin/dkr/twelf/twelf-server ; chmod 755 twelf-server ; cp twelf-server /usr/local/bin
#  ## SCMUTILS ###
RUN cd /code ; mkdir scmutils ; cd scmutils ; wget -nv http://groups.csail.mit.edu/mac/users/gjs/6946/scmutils-tarballs/scmutils-20160827-x86-64-gnu-linux.tar.gz ; cd /usr/local ; tar -xvf /code/scmutils/scmutils-20160827-x86-64-gnu-linux.tar.gz
COPY dkr/software/mechanics-shell /usr/local/bin/mechanics-shell
#  # Java ##
#  RUN apt-get install -y openjdk-6-jdk
#  RUN apt-get install -y openjdk-7-jdk
RUN (apt-get update ;apt-get install --no-install-recommends openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 -y )
#  RUN apt-get install -y openjdk-9-jdk
#  RUN apt-get install -y openjdk-10-jdk
RUN (apt-get update ;apt-get install --no-install-recommends openjdk-11-jdk=11.0.18+10-0ubuntu1~18.04.1 -y )
#  RUN add-apt-repository -y ppa:webupd8team/java
#  RUN apt-get update
#  RUN echo "oracle-java6-installer shared/accepted-oracle-license-v1-1 select true" | debconf-set-selections;\
#      echo "oracle-java6-installer shared/accepted-oracle-license-v1-1 seen true" | debconf-set-selections;\
#      apt-get install -y oracle-java6-installer
#  RUN echo "oracle-java7-installer shared/accepted-oracle-license-v1-1 select true" | debconf-set-selections
#  RUN echo "oracle-java7-installer shared/accepted-oracle-license-v1-1 seen true" | debconf-set-selections
#  RUN apt-get install -y oracle-java7-installer
#  RUN echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections && apt-get install -y oracle-java8-installer
#  RUN echo oracle-java9-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections && apt-get install -y oracle-java9-installer
#  RUN add-apt-repository -y ppa:linuxuprising/java
#  RUN apt-get update
#  RUN echo oracle-java11-installer shared/accepted-oracle-license-v1-2 select true | /usr/bin/debconf-set-selections
#  RUN apt-get install -y oracle-java11-installer
#  # Scala ##
#  RUN apt-get install -y scala
RUN cd /code ; wget -nv http://downloads.lightbend.com/scala/2.12.8/scala-2.12.8.tgz ; tar -xzvf scala-2.12.8.tgz
RUN cd /code ; wget -nv http://downloads.lightbend.com/scala/2.11.8/scala-2.11.8.tgz ; tar -xzvf scala-2.11.8.tgz
#  RUN  cd /code;\
#       wget -nv http://scala-lang.org/files/archive/scala-2.9.3.tgz;\
#       tar -xzvf scala-2.9.3.tgz
#  # LaTeX ##
RUN (apt-get update ;apt-get install --no-install-recommends texlive-latex-base=2017.20180305-1 texlive-latex-extra=2017.20180305-2 -y )
#  # Racket ##
RUN add-apt-repository ppa:plt/racket
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends racket=6.11+dfsg1-1 -y )
#  # SMT ##
RUN (apt-get update ;apt-get install --no-install-recommends z3=4.4.1-0.3build4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends cvc4=1.5-1 -y )
#  # user runner ##
RUN (apt-get update ;apt-get install --no-install-recommends sudo=1.8.21p2-3ubuntu1.5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends dos2unix=7.3.4-3 -y )
RUN useradd -m -d /home/runner -s /bin/bash runner
#  # Install io.livecode.ch scripts ##
COPY dkr/livecode-install /usr/local/bin/livecode-install
COPY dkr/livecode-run /usr/local/bin/livecode-run
#  # From now on, everything is executed as user runner ##
ENV HOME="/home/runner"
RUN env
RUN sudo -u runner mkdir /home/runner/bin
RUN sudo -u runner mkdir /home/runner/files
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
