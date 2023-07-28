FROM ubuntu:xenial
MAINTAINER Hyeshik Chang <hyeshik@snu.ac.kr>
ENV LC_ALL="C"
ENV PATH="/opt/tailseeker/bin:$PATH"
RUN perl -pi -e 's,http://archive.ubuntu.com/ubuntu/,http://ftp.daum.net/ubuntu/,g' /etc/apt/sources.list
RUN apt-get update -y \
 && apt-get -y upgrade \
 && apt-get install --no-install-recommends whiptail=0.52.18-1ubuntu2 git=1:2.7.4-0ubuntu1.10 pkg-config=0.29.1-0ubuntu1 gcc=4:5.3.1-1ubuntu1 wget=1.17.1-1ubuntu1.5 make=4.1-6 -y \
 && apt-get install --no-install-recommends libblas-dev=3.6.0-2ubuntu2 liblapack-dev=3.6.0-2ubuntu2 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 libbz2-dev=1.0.6-8ubuntu0.2 gfortran=4:5.3.1-1ubuntu1 -y \
 && apt-get install --no-install-recommends file=1:5.25-2ubuntu1.4 python3=3.5.1-3 python3-matplotlib=1.5.1-1ubuntu1 python3-dev=3.5.1-3 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Download tailseeker
RUN mkdir -p /opt ; cd /opt \
 && git clone https://github.com/hyeshik/tailseeker.git
#   Install GSNAP/GMAP
RUN cd /tmp \
 && wget -q http://research-pub.gene.com/gmap/src/gmap-gsnap-2016-11-07.tar.gz \
 && tar -xzf gmap-gsnap-2016-11-07.tar.gz \
 && cd gmap-2016-11-07 \
 && env MAX_STACK_READLENGTH=512 ./configure --prefix=/opt/tailseeker \
 && make -j 8 \
 && make install \
 && cd / \
 && rm -rf /tmp/gmap-gsnap-2016-11-07.tar.gz gmap-2016-11-07
#   Install the STAR aligner
RUN cd /tmp \
 && wget -q https://github.com/alexdobin/STAR/archive/2.5.2b.tar.gz \
 && tar -xzf 2.5.2b.tar.gz \
 && cd STAR-2.5.2b/source \
 && make -j 8 STAR STARlong \
 && install -s -m 0755 -t /opt/tailseeker/bin STAR STARlong \
 && cd / \
 && rm -rf /tmp/2.5.2b.tar.gz STAR-2.5.2b
#   Install the AYB base-caller
RUN cd /tmp \
 && git clone https://github.com/hyeshik/AYB2.git \
 && cd AYB2/src \
 && make -j 8 \
 && install -s -m 0755 -t /opt/tailseeker/bin ../bin/AYB \
 && cd / \
 && rm -rf /tmp/AYB2
#   Install Python modules for tailseeker
RUN (wget -q https://bootstrap.pypa.io/ez_setup.py -O - | python3 ) \
 && easy_install pip \
 && rm -f setuptools*.zip \
 && pip install cython==0.29.34 --no-cache-dir --upgrade \
 && pip install snakemake==7.25.0 colormath==3.0.0 numpy==1.24.2 scipy==1.10.1 pandas==2.0.0 PyYAML==6.0 biopython==1.81 feather-format==0.4.1 XlsxWriter==3.1.0 --no-cache-dir --upgrade
#   Install htslib
RUN cd /tmp \
 && wget -q https://github.com/samtools/htslib/releases/download/1.3.1/htslib-1.3.1.tar.bz2 \
 && tar -xjf htslib-1.3.1.tar.bz2 \
 && cd htslib-1.3.1 \
 && ./configure --prefix=/opt/tailseeker \
 && make -j 8 \
 && make install \
 && cd / \
 && rm -rf /tmp/htslib-1.3.1.tar.bz2 /tmp/htslib-1.3.1
#   Install samtools
RUN cd /tmp \
 && wget -q https://github.com/samtools/samtools/releases/download/1.3.1/samtools-1.3.1.tar.bz2 \
 && tar -xjf samtools-1.3.1.tar.bz2 \
 && cd samtools-1.3.1 \
 && ./configure --prefix=/opt/tailseeker --with-htslib=/opt/tailseeker --without-curses \
 && make -j 8 \
 && make install \
 && cd / \
 && rm -rf /tmp/samtools-1.3.1.tar.bz2 /tmp/samtools-1.3.1
#   Install Heng Li's seqtk
RUN cd /tmp \
 && git clone https://github.com/lh3/seqtk.git \
 && cd seqtk \
 && make \
 && install -s -m 0755 -t /opt/tailseeker/bin seqtk \
 && cd / \
 && rm -rf /tmp/seqtk
#   Install bedtools
RUN cd /tmp \
 && wget -q https://github.com/arq5x/bedtools2/releases/download/v2.26.0/bedtools-2.26.0.tar.gz \
 && tar -xzf bedtools-2.26.0.tar.gz \
 && cd bedtools2 \
 && perl -pi -e 's/python/python3/' Makefile \
 && make -j 8 \
 && install -s -m 0755 -t /opt/tailseeker/bin bin/bedtools \
 && cd / \
 && rm -rf /tmp/bedtools-2.26.0.tar.gz /tmp/bedtools2
#   Install parallel
RUN cd /tmp \
 && wget -q http://ftp.gnu.org/gnu/parallel/parallel-20161122.tar.bz2 \
 && tar -xjf parallel-20161122.tar.bz2 \
 && cd parallel-20161122 \
 && ./configure --prefix=/opt/tailseeker \
 && make \
 && make install \
 && cd / \
 && rm -rf /tmp/parallel-20161122.tar.bz2 /tmp/parallel-20161122
#   Install tailseeker 3
RUN cd /opt/tailseeker \
 && env TAILSEEKER_USE_AYB=yes TAILSEEKER_ANALYSIS_LEVEL=3 TAILSEEKER_USE_GSNAP=yes TAILSEEKER_GSNAP_SENSITIVITY=yes TAILSEEKER_BINDIR=/opt/tailseeker/bin PKG_CONFIG_PATH=/opt/tailseeker/lib/pkgconfig sh setup.sh
#   Prepare data directories.
ENV LD_LIBRARY_PATH="/opt/tailseeker/lib"
RUN mkdir -p -m 0777 /work /data
WORKDIR /work
ENTRYPOINT ["/opt/tailseeker/bin/tailseq-docker-wrap"]
#   ex: ts=8 sts=4 sw=4 et
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
