FROM ubuntu:14.04
ENV DEBIAN_FRONTEND="noninteractive"
RUN :
#  Runit
RUN (apt-get update ;apt-get install --no-install-recommends runit=2.1.1-6.2ubuntu3 -y )
CMD /usr/sbin/runsvdir-start
#  SSHD
RUN (apt-get update ;apt-get install --no-install-recommends openssh-server=1:6.6p1-2ubuntu2.13 -y ) \
 && mkdir -p /var/run/sshd \
 && echo 'root:root' | chpasswd
RUN sed -i "s/session.*required.*pam_loginuid.so/#session required pam_loginuid.so/" /etc/pam.d/sshd
RUN sed -i "s/PermitRootLogin without-password/#PermitRootLogin without-password/" /etc/ssh/sshd_config
#  Utilities
RUN (apt-get update ;apt-get install --no-install-recommends vim=2:7.4.052-1ubuntu3.1 less=458-2 net-tools=1.60-25ubuntu2.1 inetutils-ping=2:1.9.2-1 curl=7.35.0-1ubuntu2.20 git=1:1.9.1-1ubuntu0.10 telnet=0.17-36build2 nmap=6.40-0.2ubuntu1 socat=1.7.2.3-1 dnsutils=1:9.9.5.dfsg-3ubuntu0.19 netcat=1.10-40 tree=1.6.0-1 htop=1.0.2-3 unzip=6.0-9ubuntu1.5 sudo=1.8.9p5-1ubuntu1.4 software-properties-common=0.92.37.8 -y )
#  Required by Python packages
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y build-essential python-dev python-pip liblapack-dev libatlas-dev gfortran libfreetype6 libfreetype6-dev libpng12-dev python-lxml libyaml-dev g++ libffi-dev
#  0MQ
RUN cd /tmp \
 && wget http://download.zeromq.org/zeromq-4.0.3.tar.gz \
 && tar xvfz zeromq-4.0.3.tar.gz \
 && cd zeromq-4.0.3 \
 && ./configure \
 && make install \
 && ldconfig
#  Upgrade pip
RUN pip install setuptools==67.6.1 -U
RUN pip install pip==23.1 -U
#  matplotlib needs latest distribute
RUN pip install distribute==0.7.3 -U
#  IPython
RUN pip install ipython==8.12.0
ENV IPYTHONDIR="/ipython"
RUN mkdir /ipython \
 && ipython profile create nbserver
#  NumPy v1.7.1 is required for Numba
RUN pip install numpy==1.7.1
#  Pandas
RUN pip install pandas==2.0.0
#  Optional
RUN pip install cython==0.29.34
RUN pip install jinja2==3.1.2 pyzmq==25.0.2 tornado==6.2
RUN pip install numexpr==2.8.4 bottleneck==1.3.7 scipy==1.10.1 pygments==2.15.0
RUN (apt-get update ;apt-get install --no-install-recommends pkg-config=0.26-1ubuntu4 )
RUN pip install matplotlib==3.7.1
RUN pip install sympy==1.11.1 pymc==5.3.0
RUN pip install patsy==0.5.3
RUN pip install statsmodels==0.13.5
RUN pip install beautifulsoup4==4.12.2 html5lib==1.1
#  Pattern
RUN pip install --allow-external pattern
#  NLTK
RUN pip install pyyaml==6.0 nltk==3.8.1
#  Networkx
RUN pip install networkx==3.1
#  LLVM and Numba
RUN cd /tmp \
 && wget http://llvm.org/releases/3.2/llvm-3.2.src.tar.gz \
 && tar zxvf llvm-3.2.src.tar.gz \
 && cd llvm-3.2.src \
 && ./configure --enable-optimized \
 && REQUIRES_RTTI=1 make install \
 && pip install llvmpy==0.12.7 \
 && pip install llvmmath==0.1.1 \
 && pip install numba==0.56.4
#  Biopython
RUN pip install biopython==1.81
#  Bokeh
#  RUN pip install requests bokeh
#  Install R 3+
RUN echo 'deb http://cran.rstudio.com/bin/linux/ubuntu trusty/' > /etc/apt/sources.list.d/r.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends r-base=3.0.2-1ubuntu1 -y )
#  Rmagic
RUN pip install rpy2==3.5.10
#  Vincent
RUN pip install vincent==0.4.4
#  Add runit services
COPY sv /etc/service
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
