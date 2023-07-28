#  ###########################################################
#   Dockerfile for ENCODE DCC chip-seq-pipeline
#   Based on Ubuntu 16.04
#  ###########################################################
#   IMPORTANT!
#   If you install python2/3 packages using pip/pip3
#    and not sure about math library dependencies like BLAS and numpy,
#    then install with --no-dependencies
#   Set the base image to Ubuntu 16.04
#  FROM ubuntu:16.04
FROM ubuntu@sha256:e4a134999bea4abb4a27bc437e6118fdddfb172e1b9d683129b74d254af51675
#   File Author / Maintainer
MAINTAINER Jin Lee
#   Update the repository sources list
#   Install base packages: git, python2/3, java, R
RUN apt-get update \
 && apt-get install --no-install-recommends libncurses5-dev=6.4-2 libncursesw5-dev=6.4-2 libcurl4-openssl-dev=7.88.1-7ubuntu1 libfreetype6-dev=2.12.1+dfsg-4 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 python python-setuptools python-pip python3=3.11.2-1 python3-setuptools=66.1.1-1 python3-pip=23.0.1+dfsg-1 git=1:2.39.2-1ubuntu1 wget=1.21.3-1ubuntu1 unzip=6.0-27ubuntu1 ghostscript=10.0.0~dfsg1-0ubuntu1 pkg-config=1.8.1-1ubuntu2 libboost-dev=1.74.0.3ubuntu7 r-base-core=4.2.2.20221110-2build1 default-jre=2:1.17-74 apt-transport-https=2.6.0 tabix=1.16+ds-3 -y \
 && rm -rf /var/lib/apt/lists/*
#   Install Intel MKL for BLAS
RUN wget https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS-2019.PUB \
 && apt-key add GPG-PUB-KEY-INTEL-SW-PRODUCTS-2019.PUB \
 && rm -rf GPG-PUB-KEY-INTEL-SW-PRODUCTS-2019.PUB \
 && sh -c 'echo deb https://apt.repos.intel.com/mkl all main > /etc/apt/sources.list.d/intel-mkl.list' \
 && apt-get update \
 && apt-get install --no-install-recommends intel-mkl-64bit-2018.0-033 -y \
 && rm -rf /var/lib/apt/lists/*
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/opt/intel/compilers_and_libraries_2018.0.128/linux/mkl/lib/intel64_lin"
#   Install basic python2/3 packages
RUN pip install common==0.1.2 python-dateutil==2.8.2 cython==0.27.3 --no-cache-dir \
 && pip3 install --no-cache-dir common python-dateutil cython==0.27.3
#   Make directory for all softwares
RUN mkdir /software
WORKDIR /software
ENV PATH="/software:${PATH}"
#   Install numpy 1.11.3 (python2/3, linked with MKL)
RUN git clone --branch v1.11.3 https://github.com/numpy/numpy \
 && cd numpy \
 && /bin/bash -c 'echo -e "[mkl]\nlibrary_dirs = /opt/intel/compilers_and_libraries_2018/linux/mkl/lib/intel64\ninclude_dirs = /opt/intel/compilers_and_libraries_2018/linux/mkl/include\nmkl_libs = mkl_rt\nlapack_libs =" > site.cfg' \
 && python setup.py install \
 && python3 setup.py install \
 && cd ../ \
 && rm -rf numpy*
#   Install scipy 1.0.0 (python2/3)
RUN git clone --branch v1.0.0 --single-branch https://github.com/scipy/scipy \
 && cd scipy \
 && python setup.py install \
 && python3 setup.py install \
 && cd ../ \
 && rm -rf scipy*
#   Install matplotlib 1.5.1 (python2/3)
RUN git clone --branch v1.5.1 --single-branch https://github.com/matplotlib/matplotlib \
 && cd matplotlib \
 && python setup.py install \
 && python3 setup.py install \
 && cd ../ \
 && rm -rf matplotlib*
#   Install MACS2 2.1.1.20160309 (python2)
RUN pip install macs2==2.1.1.20160309 --no-cache-dir --no-dependencies
#   Install IDR 2.0.4.2 (python3)
RUN git clone --branch 2.0.4.2 --single-branch https://github.com/kundajelab/idr \
 && cd idr \
 && python3 setup.py install \
 && cd ../ \
 && rm -rf idr*
#   Install samtools 1.2
RUN git clone --branch 1.2 --single-branch https://github.com/samtools/samtools.git \
 && git clone --branch 1.2 --single-branch https://github.com/samtools/htslib.git \
 && cd samtools \
 && make \
 && make install \
 && cd ../ \
 && rm -rf samtools* htslib*
#   Install bedtools 2.26.0
RUN git clone --branch v2.26.0 --single-branch https://github.com/arq5x/bedtools2.git \
 && cd bedtools2 \
 && make \
 && make install \
 && cd ../ \
 && rm -rf bedtools2*
#   Install Picard 2.10.6
RUN wget https://github.com/broadinstitute/picard/releases/download/2.10.6/picard.jar \
 && chmod +x picard.jar
#   Install sambamba 0.6.6
RUN wget https://github.com/lomereiter/sambamba/releases/download/v0.6.6/sambamba_v0.6.6_linux.tar.bz2 \
 && tar -xvjf sambamba_v0.6.6_linux.tar.bz2 \
 && mv sambamba_v0.6.6 sambamba \
 && rm -rf sambamba_*
#   Install R packages
RUN echo "r <- getOption('repos'); r['CRAN'] <- 'http://cran.r-project.org'; options(repos = r);" > ~/.Rprofile \
 && Rscript -e "install.packages('snow')" \
 && Rscript -e "install.packages('snowfall')" \
 && Rscript -e "install.packages('bitops')" \
 && Rscript -e "install.packages('caTools')" \
 && Rscript -e "source('http://bioconductor.org/biocLite.R'); biocLite('Rsamtools')"
#   Install R package spp 1.13 (required for phantompeakqualtools)
RUN wget https://github.com/hms-dbmi/spp/archive/1.13.tar.gz \
 && Rscript -e "install.packages('./1.13.tar.gz')" \
 && rm -f 1.13.tar.gz
#   Install phantompeakqualtools 1.2
RUN wget https://github.com/kundajelab/phantompeakqualtools/archive/1.2.tar.gz \
 && tar -xvf 1.2.tar.gz \
 && rm -f 1.2.tar.gz
ENV PATH="/software/phantompeakqualtools-1.2:${PATH}"
#   Install Bwa 0.7.13
RUN git clone --branch v0.7.13 --single-branch https://github.com/lh3/bwa.git \
 && cd bwa \
 && make \
 && cp bwa /usr/local/bin/ \
 && cd ../ \
 && rm -rf bwa*
#   Install pysam 0.9.0 (python2)
RUN git clone --branch v0.9.0 --single-branch https://github.com/pysam-developers/pysam \
 && cd pysam \
 && python setup.py install \
 && cd ../ \
 && rm -rf pysam*
#   Install pyBigwig 0.2.8 (python2)
RUN git clone --branch 0.2.8 --single-branch https://github.com/deeptools/pyBigWig \
 && cd pyBigWig \
 && python setup.py install \
 && cd ../ \
 && rm -rf pyBigWig*
#   Install deeptools 2.5.4 (python2)
RUN pip install deeptools==2.5.4 --no-cache-dir --no-dependencies
#   Install pyfaidx (for building genome data)
RUN pip install pyfaidx==0.4.7.1 --no-cache-dir
#   Install bgzip/tabix for Wash U browser track (hammock type)
#  RUN apt-get update && apt-get install -y tabix && rm -rf /var/lib/apt/lists/*
#   Install UCSC tools (v377)
RUN git clone https://github.com/ENCODE-DCC/kentUtils_bin_v377
ENV PATH="${PATH}:/software/kentUtils_bin_v377/bin"
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/software/kentUtils_bin_v377/lib"
#   Prevent conflict with locally installed python outside of singularity container
ENV PYTHONNOUSERSITE="True"
#   Get ENCODE chip-seq-pipeline container repository
#   This COPY assumes the build context is the root of the chip-seq-pipeline repo
#   and it gets whatever is checked out plus local modifications
#   so the buildling command should:
#   cd [GIT_REPO_DIR] && docker build -f docker_image/Dockerfile .
RUN mkdir -p chip-seq-pipeline/src
COPY src chip-seq-pipeline/src/
COPY chip.wdl chip-seq-pipeline/
ENV PATH="/software/chip-seq-pipeline:/software/chip-seq-pipeline/src:${PATH}"
#   make some temporary directories
RUN for i in $( seq 0 9 ;); do mkdir -p /mnt/ext_$i ; done
#  ENTRYPOINT ["/bin/bash","-c"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
