FROM ubuntu:14.04
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update
# Runit
RUN apt-get install --no-install-recommends runit -y
CMD /usr/sbin/runsvdir-start
# SSHD
RUN apt-get install --no-install-recommends openssh-server -y \
 && mkdir -p /var/run/sshd \
 && echo 'root:root' | chpasswd
RUN sed -i "s/session.*required.*pam_loginuid.so/#session required pam_loginuid.so/" /etc/pam.d/sshd
RUN sed -i "s/PermitRootLogin without-password/#PermitRootLogin without-password/" /etc/ssh/sshd_config
# Utilities
RUN apt-get install --no-install-recommends vim less net-tools inetutils-ping curl git telnet nmap socat dnsutils netcat tree htop unzip sudo software-properties-common -y
# Required by Python packages
RUN DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends install -y build-essential python-dev python-pip liblapack-dev libatlas-dev gfortran libfreetype6 libfreetype6-dev libpng12-dev python-lxml libyaml-dev g++ libffi-dev
# 0MQ
RUN cd /tmp \
 && wget http://download.zeromq.org/zeromq-4.0.3.tar.gz \
 && tar xvfz zeromq-4.0.3.tar.gz \
 && cd zeromq-4.0.3 \
 && ./configure \
 && make install \
 && ldconfig
# Upgrade pip
RUN pip install setuptools -U
RUN pip install pip -U
# matplotlib needs latest distribute
RUN pip install distribute -U
# IPython
RUN pip install ipython
ENV IPYTHONDIR="/ipython"
RUN mkdir /ipython \
 && ipython profile create nbserver
# NumPy v1.7.1 is required for Numba
RUN pip install numpy==1.7.1
# Pandas
RUN pip install pandas
# Optional
RUN pip install cython
RUN pip install jinja2 pyzmq tornado
RUN pip install numexpr bottleneck scipy pygments
RUN apt-get install --no-install-recommends pkg-config
RUN pip install matplotlib
RUN pip install sympy pymc
RUN pip install patsy
RUN pip install statsmodels
RUN pip install beautifulsoup4 html5lib
# Pattern
RUN pip install --allow-external pattern
# NLTK
RUN pip install pyyaml nltk
# Networkx
RUN pip install networkx
# LLVM and Numba
RUN cd /tmp \
 && wget http://llvm.org/releases/3.2/llvm-3.2.src.tar.gz \
 && tar zxvf llvm-3.2.src.tar.gz \
 && cd llvm-3.2.src \
 && ./configure --enable-optimized \
 && REQUIRES_RTTI=1 make install \
 && pip install llvmpy \
 && pip install llvmmath \
 && pip install numba
# Biopython
RUN pip install biopython
# Bokeh
# RUN pip install requests bokeh
# Install R 3+
RUN echo 'deb http://cran.rstudio.com/bin/linux/ubuntu trusty/' > /etc/apt/sources.list.d/r.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
RUN apt-get update
RUN apt-get install --no-install-recommends r-base -y
# Rmagic
RUN pip install rpy2
# Vincent
RUN pip install vincent
# Add runit services
COPY sv /etc/service
