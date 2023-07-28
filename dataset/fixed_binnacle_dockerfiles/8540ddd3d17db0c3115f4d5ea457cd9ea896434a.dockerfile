#   ==============================================================================
#
#   This Dockerfile contains all the commands required to set up an environment
#   for using EstimationPy
#   
FROM ubuntu:14.04
MAINTAINER lbl-srg
#   Avoid interaction
ENV DEBIAN_FRONTEND="noninteractive"
#   =========== Basic Configuration ===============================================
#   Update the system
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 git=1:1.9.1-1ubuntu0.10 python=2.7.5-5ubuntu3 python-dev=2.7.5-5ubuntu3 python-setuptools=3.3-1ubuntu2 make=3.81-8.2ubuntu3 cmake=2.8.12.2-0ubuntu3 gfortran=4:4.8.2-1ubuntu6 -y )
#   Install pip for managing python packages
RUN (apt-get update ;apt-get install --no-install-recommends python-pip=1.5.4-1ubuntu4 python-lxml=3.3.3-1ubuntu0.2 -y )
RUN pip install cython==0.29.34
RUN (apt-get update ;apt-get install --no-install-recommends python-lxml=3.3.3-1ubuntu0.2 -y )
#   Add folders that will contains code before and after installation
RUN mkdir -p /home/docker/to_install
RUN mkdir -p /home/docker/installed
#   Create an user and an environmental variable associated to it
RUN adduser --disabled-password --gecos '' docker
RUN adduser docker sudo
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
#   =========== Install JAVA =======================================================
#   Install Java (Open JDK, version 7)
RUN (apt-get update ;apt-get install --no-install-recommends openjdk-7-jdk=7u211-2.6.17-0ubuntu0.1 -y ) \
 && rm -rf /var/lib/apt/lists/*
#   Define commonly used JAVA_HOME variable
ENV JAVA_HOME="/usr/lib/jvm/java-7-openjdk-amd64"
RUN echo "export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64" >> /root/.bashrc
RUN echo "export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64" >> /home/docker/.bashrc
#   =========== Install PyFMI dependencies ======================================================
#   Install Sundials ODE, DAE
COPY ./softwares/sundials-2.5.0.tar.gz /home/docker/to_install/
RUN cd /home/docker/to_install \
 && tar xzvf ./sundials-2.5.0.tar.gz
WORKDIR /home/docker/to_install/sundials-2.5.0
RUN mkdir -p build
WORKDIR /home/docker/to_install/sundials-2.5.0/build
RUN ../configure CFLAGS="-fPIC" --prefix=/home/docker/installed/sundials-2.5.0
RUN make \
 && make install \
 && make clean
WORKDIR /home/docker
#   Install BLAS and LAPACK
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends libblas3gf=1.2.20110419-7 libblas-doc=1.2.20110419-7 libblas-dev=1.2.20110419-7 -y )
RUN (apt-get update ;apt-get install --no-install-recommends liblapack3gf=3.5.0-2ubuntu1 liblapack-doc=3.5.0-2ubuntu1 liblapack-dev=3.5.0-2ubuntu1 -y )
#   Other dependencies required before installing pylab and Matplotlib
RUN (apt-get update ;apt-get install --no-install-recommends pkgconf=0.9.4-1 libpng-dev libfreetype6-dev=2.5.2-1ubuntu2.8 -y )
#   Install Numpy, Matplotlib, SciPy and Pandas
RUN pip install numpy==1.24.2
RUN (apt-get update ;apt-get install --no-install-recommends python-matplotlib=1.3.1-1ubuntu5.1 -y )
RUN pip install scipy==1.10.1
RUN pip install pandas==2.0.0
#   Install svn and git
RUN (apt-get update ;apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 subversion=1.8.8-1ubuntu3.3 -y )
#   Install assimulo
RUN cd /home/docker/to_install \
 && svn checkout https://svn.jmodelica.org/assimulo/tags/Assimulo-2.8/
WORKDIR /home/docker/to_install/Assimulo-2.8
RUN python setup.py install --sundials-home=/home/docker/installed/sundials-2.5.0/ --blas-home=/usr/lib/lapack/
#   Install FMILib
RUN cd /home/docker/to_install/ \
 && svn checkout https://svn.jmodelica.org/FMILibrary/tags/2.0.1/ FMILibrary-2.0.1
RUN cd /home/docker/to_install \
 && ls -la
WORKDIR /home/docker/to_install/FMILibrary-2.0.1/
RUN ls -la
RUN mkdir -p build-fmil
WORKDIR /home/docker/to_install/FMILibrary-2.0.1/build-fmil
RUN cmake -DFMILIB_INSTALL_PREFIX=/home/docker/installed/FMIlib2.0.1 /home/docker/to_install/FMILibrary-2.0.1
RUN make install test
#   Finally install PyFMI
RUN cd /home/docker/to_install/ \
 && svn checkout https://svn.jmodelica.org/PyFMI/tags/PyFMI-2.0b1/
WORKDIR /home/docker/to_install/PyFMI-2.0b1
RUN python setup.py install --fmil-home=/home/docker/installed/FMIlib2.0.1/
#   Create "dummy" Dymola license file. It is also required by FMUs that are exported in
#   binary export mode
RUN mkdir -p /home/docker/.dynasim
RUN touch /home/docker/.dynasim/dymola.lic
RUN echo "SERVER yourserver.com ANY" >> /home/docker/.dynasim/dymola.lic
RUN echo "VENDOR dynasim" >> /home/docker/.dynasim/dymola.lic
RUN echo "USE_SERVER" >> /home/docker/.dynasim/dymola.lic
ENV DYMOLA_RUNTIME_LICENSE="\"/home/ubuntu/.dynasim/dymola.lic\""
RUN echo "export DYMOLA_RUNTIME_LICENSE=/home/ubuntu/.dynasim/dymola.lic" >> /root/.bashrc
RUN echo "export DYMOLA_RUNTIME_LICENSE=/home/ubuntu/.dynasim/dymola.lic" >> /home/docker/.bashrc
#   Install EstimationPy
RUN cd /home/docker/to_install \
 && git clone https://github.com/lbl-srg/EstimationPy.git \
 && cd ./EstimationPy \
 && python setup.py install
WORKDIR /home/docker
#   Install ipython notebook
RUN pip install "ipython[notebook]"
#   Change ownership of the content of /home/docker
RUN chown -R docker:docker /home/docker/
#   Change user to docker
USER docker
#   Create folder that will be used as a shared volume
RUN mkdir -p /home/docker/shared_folder
VOLUME ["/home/docker/shared_folder"]
#   Create environmental variables for the display
ENV DISPLAY=":0.0"
ENV USER="docker"
#   Expose the port where the ipython notebook server will listen
EXPOSE 8888/tcp
#   Command to run by default in detached mode
CMD ipython notebook --ip="0.0.0.0" --port=8888 --notebook-dir=/home/docker/shared_folder --no-browser
# Please add your HEALTHCHECK here!!!
