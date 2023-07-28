#   Author: Marco Bonvini
#   email: bonvini.m@gmail.com
#
FROM ubuntu:16.04
MAINTAINER MarcoBonvini bonvini.m@gmail.com
#   Avoid interaction
ENV DEBIAN_FRONTEND="noninteractive"
#   =========== Basic Configuration ======================================================
#   Update the system
RUN apt-get update -y \
 && apt-get install --no-install-recommends sudo=1.8.16-0ubuntu1.10 build-essential=12.1ubuntu2 git=1:2.7.4-0ubuntu1.10 python=2.7.12-1~16.04 python-dev=2.7.12-1~16.04 python-setuptools=20.7.0-1 make=4.1-6 g++=4:5.3.1-1ubuntu1 cmake=3.5.1-1ubuntu3 gfortran=4:5.3.1-1ubuntu1 ipython=2.4.1-1 swig=3.0.8-0ubuntu3 ant=1.9.6-1ubuntu1.1 python-numpy=1:1.11.0-1ubuntu1 python-scipy=0.17.0-1 python-matplotlib=1.5.1-1ubuntu1 cython=0.23.4-0ubuntu5 python-lxml=3.5.0-1ubuntu0.4 python-nose=1.3.7-1 python-jpype=0.5.4.2-3 libboost-dev=1.58.0.1ubuntu1 jcc=2.21-1.1 git=1:2.7.4-0ubuntu1.10 subversion=1.9.3-2ubuntu1.3 wget=1.17.1-1ubuntu1.5 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 pkg-config=0.29.1-0ubuntu1 clang=1:3.8-33ubuntu3.1 -y
#   ========== Install pip for managing python packages ==================================
RUN apt-get install --no-install-recommends python-pip=8.1.1-2ubuntu0.6 python-lxml=3.5.0-1ubuntu0.4 -y \
 && pip install cython==0.29.34
#   ========== Create an user and environmental variables associated to it ===============
RUN adduser --disabled-password --gecos '' docker
RUN adduser docker sudo
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
#   ========= Add folders that will contains code before and after installation ==========
RUN mkdir -p /home/docker/to_install \
 && mkdir -p /home/docker/installed/Ipopt
#   ========= Install JAVA ===============================================================
RUN apt-get install --no-install-recommends openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 -y \
 && rm -rf /var/lib/apt/lists/*
#   Define JAVA_HOME envirponmental variable
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"
RUN echo "export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64" >> /root/.bashrc \
 && echo "export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64" >> /home/docker/.bashrc
#   ======== Install BLAS and LAPACK =====================================================
RUN apt-get update -y \
 && apt-get install --no-install-recommends apt-utils=1.2.35 -y \
 && apt-get install --no-install-recommends libblas-dev=3.6.0-2ubuntu2 liblapack-dev=3.6.0-2ubuntu2 -y
#   ======== Install numpy, scipy, Matplotlib ============================================
RUN apt-get install --no-install-recommends pkgconf=0.9.12-1 libpng-dev libfreetype6-dev=2.6.1-0.1ubuntu2.5 -y \
 && pip install numpy==1.24.2 \
 && apt-get install --no-install-recommends python-matplotlib=1.5.1-1ubuntu1 -y \
 && pip install scipy==1.10.1
#   ======== Start IPOPT installation ====================================================
#   Retrieve and copy all the dependencies needed by Ipopt
WORKDIR /home/docker/to_install/Ipopt
RUN wget http://www.coin-or.org/download/source/Ipopt/Ipopt-3.12.4.tgz
RUN tar xvf ./Ipopt-3.12.4.tgz
WORKDIR /home/docker/to_install/Ipopt/Ipopt-3.12.4/ThirdParty/Blas
RUN ./get.Blas
WORKDIR /home/docker/to_install/Ipopt/Ipopt-3.12.4/ThirdParty/Lapack
RUN ./get.Lapack
WORKDIR /home/docker/to_install/Ipopt/Ipopt-3.12.4/ThirdParty/Mumps
RUN ./get.Mumps
WORKDIR /home/docker/to_install/Ipopt/Ipopt-3.12.4/ThirdParty/Metis
RUN ./get.Metis
#   Configure and compile Ipopt
WORKDIR /home/docker/to_install/Ipopt/Ipopt-3.12.4/
RUN mkdir build
WORKDIR /home/docker/to_install/Ipopt/Ipopt-3.12.4/build
RUN ../configure --prefix=/home/docker/installed/Ipopt \
 && make \
 && make install
#   ======== Start JModelica.org installation ===========================================0
#   Intall autoconf which is called by the casADi installation
RUN apt-get install --no-install-recommends autoconf=2.69-9 -y
#   Checkout the JModelica.org source code
RUN mkdir -p /home/docker/installed/JModelica
WORKDIR /home/docker/to_install
RUN svn co https://svn.jmodelica.org/trunk JModelica.org
WORKDIR /home/docker/to_install/JModelica.org
RUN mkdir build
WORKDIR /home/docker/to_install/JModelica.org/build
RUN ../configure --prefix=/home/docker/installed/JModelica --with-ipopt=/home/docker/installed/Ipopt
RUN make \
 && make install \
 && make install_casadi \
 && make casadi_interface
#   Define the environmental variables needed by JModelica
#   JModelica.org supports the following environment variables:
#
#   - JMODELICA_HOME containing the path to the JModelica.org installation
#     directory (again, without spaces or ~ in the path).
#   - PYTHONPATH containing the path to the directory $JMODELICA_HOME/Python.
#   - JAVA_HOME containing the path to a Java JRE or SDK installation.
#   - IPOPT_HOME containing the path to an Ipopt installation directory.
#   - LD_LIBRARY_PATH containing the path to the $IPOPT_HOME/lib directory
#     (Linux only.)
#   - MODELICAPATH containing a sequence of paths representing directories
#     where Modelica libraries are located, separated by colons.
ENV JMODELICA_HOME="/home/docker/installed/JModelica"
ENV IPOPT_HOME="/home/docker/installed/Ipopt"
ENV CPPAD_HOME="/home/docker/installed/JModelica/ThirdParty/CppAD/"
ENV SUNDIALS_HOME="/home/docker/installed/JModelica/ThirdParty/Sundials"
ENV PYTHONPATH="/home/docker/installed/JModelica/Python/:"
ENV LD_LIBRARY_PATH="/home/docker/installed/Ipopt/lib/:/home/docker/installed/JModelica/ThirdParty/Sundials/lib:/home/docker/installed/JModelica/ThirdParty/CasADi/lib"
ENV SEPARATE_PROCESS_JVM="/usr/lib/jvm/java-8-openjdk-amd64/"
ENV MODELICAPATH="/home/docker/installed/JModelica/ThirdParty/MSL:/home/docker/modelica"
#   ============ Expose ports ============================================================
EXPOSE 8888/tcp
#   ============ Install IPython notebook ================================================
RUN apt-get install --no-install-recommends ipython=2.4.1-1 ipython-notebook=2.4.1-1 -y
#   ============ Set some environmental vars and change user =============================
USER docker
RUN mkdir /home/docker/modelica \
 && mkdir /home/docker/ipynotebooks
ENV USER="docker"
ENV DISPLAY=":0.0"
WORKDIR /home/docker/
# Please add your HEALTHCHECK here!!!
