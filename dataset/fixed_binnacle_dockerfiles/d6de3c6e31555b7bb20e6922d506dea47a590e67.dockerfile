#   Author: Marco Bonvini
#   email: bonvini.m@gmail.com
#
FROM ubuntu:18.04
MAINTAINER MarcoBonvini bonvini.m@gmail.com
#   Avoid interaction
ENV DEBIAN_FRONTEND="noninteractive"
#   =========== Basic Configuration ======================================================
#   Update the system
RUN apt-get update -y \
 && apt-get install --no-install-recommends sudo=1.8.21p2-3ubuntu1.5 build-essential=12.4ubuntu1 git=1:2.17.1-1ubuntu0.17 python=2.7.15~rc1-1 python-dev=2.7.15~rc1-1 python-setuptools=39.0.1-2ubuntu0.1 make=4.1-9.1ubuntu1 g++=4:7.4.0-1ubuntu2.3 cmake=3.10.2-1ubuntu2.18.04.2 gfortran=4:7.4.0-1ubuntu2.3 ipython=5.5.0-1 swig=3.0.12-1 ant=1.10.5-3~18.04 python-numpy=1:1.13.3-2ubuntu1 python-scipy=0.19.1-2ubuntu1 python-matplotlib=2.1.1-2ubuntu3 cython=0.26.1-0.4 python-lxml=4.2.1-1ubuntu0.6 python-nose=1.3.7-3 python-jpype=0.6.2+dfsg-2 libboost-dev=1.65.1.0ubuntu1 jcc=2.22-1 git=1:2.17.1-1ubuntu0.17 subversion=1.9.7-4ubuntu1.1 wget=1.19.4-1ubuntu2.2 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 pkg-config=0.29.1-0ubuntu2 clang=1:6.0-41~exp5~ubuntu1 -y
#   ========== Install pip for managing python packages ==================================
RUN apt-get install --no-install-recommends python-pip=9.0.1-2.3~ubuntu1.18.04.8 python-lxml=4.2.1-1ubuntu0.6 -y \
 && pip install cython==0.29.34
#   ========== Create an user and environmental variables associated to it ===============
RUN adduser --disabled-password --gecos '' docker
RUN adduser docker sudo
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
#   ========= Add folders that will contains code before and after installation ==========
RUN mkdir -p /home/docker/to_install \
 && mkdir -p /home/docker/installed/Ipopt
#   ========= Install JAVA ===============================================================
RUN apt-get install --no-install-recommends openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 -y \
 && rm -rf /var/lib/apt/lists/*
#   Define JAVA_HOME envirponmental variable
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"
RUN echo "export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64" >> /root/.bashrc \
 && echo "export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64" >> /home/docker/.bashrc
#   ======== Install BLAS and LAPACK =====================================================
RUN apt-get update -y \
 && apt-get install --no-install-recommends apt-utils=1.6.14 -y \
 && apt-get install --no-install-recommends libblas-dev=3.7.1-4ubuntu1 liblapack-dev=3.7.1-4ubuntu1 -y
#   ======== Install numpy, scipy, Matplotlib ============================================
RUN apt-get install --no-install-recommends pkgconf=0.9.12-6 libpng-dev=1.6.34-1ubuntu0.18.04.2 libfreetype6-dev=2.8.1-2ubuntu2.2 -y \
 && pip install numpy==1.24.2 \
 && apt-get install --no-install-recommends python-matplotlib=2.1.1-2ubuntu3 -y \
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
RUN apt-get install --no-install-recommends autoconf=2.69-11 -y
#   Checkout the JModelica.org source code
RUN mkdir -p /home/docker/installed/JModelica
#   Thanks to Marcus Fuchs
#   https://github.com/mbonvini/ModelicaInAction/pull/4/commits/1220d7c680957943bc17ad55ff009488d0887ce0
#   This needs a nasty hack for more modern versions, since assimulo does not fit in svn ...
WORKDIR /home/docker/to_install
RUN svn co https://svn.jmodelica.org/trunk JModelica.org ; exit 0
WORKDIR /home/docker/to_install/JModelica.org/external
RUN svn co https://svn.jmodelica.org/assimulo/trunk Assimulo
WORKDIR /home/docker/to_install/JModelica.org
RUN mkdir build
WORKDIR /home/docker/to_install/JModelica.org/build
RUN ../configure --prefix=/home/docker/installed/JModelica --with-ipopt=/home/docker/installed/Ipopt
RUN make \
 && make install \
 && make install_casadi
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
#   ============ Install IPython/Jupyter notebook ========================================
RUN apt-get install --no-install-recommends ipython=5.5.0-1 -y
RUN pip install jupyter==1.0.0
#   ============ Set Jupyter password ====================================================
RUN mkdir -p /home/docker/.jupyter \
 && jupyter notebook --generate-config
RUN python -c 'import json; from notebook.auth import passwd; open("/home/docker/.jupyter/jupyter_notebook_config.json", "w").write(json.dumps({"NotebookApp":{"password": passwd("modelicainaction")}}));'
#   ============ Set some environmental vars and change user =============================
USER docker
RUN mkdir /home/docker/modelica \
 && mkdir /home/docker/ipynotebooks
ENV USER="docker"
ENV DISPLAY=":0.0"
WORKDIR /home/docker/
# Please add your HEALTHCHECK here!!!
