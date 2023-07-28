#   Set the base image to Ubuntu 14.04
FROM ubuntu:14.04
#   File Author / Maintainer
MAINTAINER Samantha Zarate
#   System packages 
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends curl=7.35.0-1ubuntu2.20 wget=1.15-1ubuntu1.14.04.5 -y )
#   Install miniconda to /miniconda
RUN curl -LO http://repo.continuum.io/miniconda/Miniconda-latest-Linux-x86_64.sh \
 && bash Miniconda-latest-Linux-x86_64.sh -p /miniconda -b \
 && rm Miniconda-latest-Linux-x86_64.sh
ENV PATH="/miniconda/bin:${PATH}"
#   RUN conda update -y conda
RUN /bin/bash -c "echo 'deb http://dnanexus-apt-prod.s3.amazonaws.com/ubuntu trusty/amd64/' > /etc/apt/sources.list.d/dnanexus.list"
RUN /bin/bash -c "echo 'deb http://dnanexus-apt-prod.s3.amazonaws.com/ubuntu trusty/all/' >> /etc/apt/sources.list.d/dnanexus.list"
RUN wget https://wiki.dnanexus.com/images/files/ubuntu-signing-key.gpg
RUN apt-key add ubuntu-signing-key.gpg
RUN apt-get update -y \
 && apt-get upgrade -y \
 && (apt-get update ;apt-get install --no-install-recommends autoconf=2.69-6 bedtools=2.17.0-1 bsdtar=3.1.2-7ubuntu2.8 build-essential=11.6ubuntu6 cmake=2.8.12.2-0ubuntu3 dx-toolkit g++=4:4.8.2-1ubuntu6 gcc=4:4.8.2-1ubuntu6 gettext=0.18.3.1-1ubuntu3.1 gfortran=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 gzip=1.6-3ubuntu1 inkscape=0.48.4-3ubuntu2 libc6=2.19-0ubuntu6.15 libcurl4-openssl-dev=7.35.0-1ubuntu2.20 libfontconfig libfreetype6-dev=2.5.2-1ubuntu2.8 libgsl0-dev=1.16+dfsg-1ubuntu1 libgtkmm-3.0-dev=3.10.1-0ubuntu2 libhdf5-serial-dev=1.8.11-5ubuntu7.1 liblzma-dev=5.1.1alpha+20120614-2ubuntu2 liblzo2-dev=2.06-1.2ubuntu1.1 libpangomm-1.4-dev=2.34.0-1ubuntu1 libpng-dev libpopt-dev=1.16-8ubuntu1 libpthread-stubs0-dev=0.3-4 librsvg2-bin=2.40.2-1 librsvg2-dev=2.40.2-1 libsqlite3-dev=3.8.2-1ubuntu2.2 libstdc++6=4.8.4-2ubuntu1~14.04.4 libx11-dev=2:1.6.2-1ubuntu2.1 libxext-dev=2:1.3.2-1ubuntu0.0.14.04.1 libxft-dev=2.3.1-2 libxpm-dev=1:3.5.10-1ubuntu0.1 libxslt1-dev=1.1.28-2ubuntu0.2 python-pip=1.5.4-1ubuntu4 sqlite3=3.8.2-1ubuntu2.2 wget=1.15-1ubuntu1.14.04.5 wkhtmltopdf=0.9.9-4 xvfb=2:1.15.1-0ubuntu2.11 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 -y --force-yes )
RUN :
RUN conda config --add channels conda-forge
RUN conda config --add channels bioconda
RUN conda config --add channels defaults
RUN conda install -c bioconda samtools
RUN conda install -c bioconda sambamba -y
RUN conda install -c bioconda bcftools -y
RUN conda install -c bcbio bx-python -y
RUN conda install -c defaults networkx -y
RUN conda install -c bioconda samblaster -y
RUN conda install gcc_linux-64 -y
RUN conda install -c bioconda manta
RUN conda update -y pyopenssl
WORKDIR /
COPY resources.tar.gz /
RUN cp -a /resources/* / \
 && rm -rf /resources/
RUN conda install -c defaults -y numpy
RUN pip install https://github.com/bioinform/breakseq2/archive/2.2.tar.gz
RUN pip install pycparser==2.21
RUN pip install asn1crypto==1.5.1
RUN pip install idna==3.4
RUN pip install ipaddress==1.0.23
RUN pip install dxpy==0.345.0
WORKDIR /root
RUN mkdir -p /home/dnanexus/in /home/dnanexus/out
WORKDIR /home/dnanexus
COPY parliament2.py .
COPY parliament2.sh .
COPY svtyper_env.yml .
RUN conda create -y --name svviz_env svviz
#   We have to use a slightly different method for 
#   svtyper as it installs software directly from git 
RUN conda env create --name svtyper_env --file svtyper_env.yml
RUN /bin/bash -c "source /etc/profile.d/dnanexus.environment.sh"
ENV PATH="${PATH}:/home/dnanexus/"
ENV PATH="${PATH}:/opt/conda/bin/"
ENV PATH="${PATH}:/usr/bin/"
ENV PYTHONPATH="${PYTHONPATH}:/opt/conda/bin/"
ENV ROOTSYS="/home/dnanexus/root"
ENV LD_LIBRARY_PATH="/usr/lib/root/lib"
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/home/dnanexus/root/lib"
ENV DYLD_LIBRARY_PATH="/usr/lib/root/lib"
ENV HTSLIB_LIBRARY_DIR="/usr/local/lib"
ENV HTSLIB_INCLUDE_DIR="/usr/local/include"
WORKDIR /home/dnanexus
RUN ["chmod", "+x", "parliament2.py"]
RUN ["chmod", "+x", "parliament2.sh"]
ENTRYPOINT ["python", "/home/dnanexus/parliament2.py"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
