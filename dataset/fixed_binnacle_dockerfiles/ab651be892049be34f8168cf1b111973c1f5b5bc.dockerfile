#   Set the base image
FROM ubuntu:16.04
ENV DEBIAN_FRONTEND="noninteractive"
#   File Author / Maintainer
MAINTAINER IDseq Team idseq-tech@chanzuckerberg.com
#   Add packages, update image, and clear cache
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 curl=7.47.0-1ubuntu2.19 wget=1.17.1-1ubuntu1.5 python-pip=8.1.1-2ubuntu0.6 python-dev=2.7.12-1~16.04 python-scipy=0.17.0-1 python-redis=2.10.5-1ubuntu1 gdebi-core=0.9.5.7ubuntu1 zip=3.0-11 unzip=6.0-20ubuntu1.1 g++=4:5.3.1-1ubuntu1 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 gcc=4:5.3.1-1ubuntu1 pkg-config=0.29.1-0ubuntu1 apt-utils=1.2.35 make=4.1-6 perl=5.22.1-9ubuntu0.9 cmake=3.5.1-1ubuntu3 libbz2-dev=1.0.6-8ubuntu0.2 -y
RUN pip install pip==23.1 --upgrade
RUN pip install redis==4.5.4 biopython==1.81 pysam==0.21.0
RUN pip install htseq==0.6.1p1
RUN apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y
RUN add-apt-repository ppa:deadsnakes/ppa
RUN apt-get update \
 && apt-get install --no-install-recommends python3.7 -y
RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.5 1
RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.7 2
WORKDIR /tmp
#   install STAR
RUN curl -L https://github.com/alexdobin/STAR/archive/2.5.3a.tar.gz | tar xz
RUN mv STAR-2.5.3a/bin/Linux_x86_64_static/* /usr/local/bin
RUN rm -rf STAR-2.5.3a
#   Compile and install bowtie
RUN wget https://sourceforge.net/projects/bowtie-bio/files/bowtie2/2.3.2/bowtie2-2.3.2-source.zip
RUN unzip bowtie2-2.3.2-source.zip
WORKDIR /tmp/bowtie2-2.3.2
RUN make -j 16 NO_TBB=1
RUN mv -t /usr/local/bin/ bowtie2 bowtie2-align-l bowtie2-align-s bowtie2-build bowtie2-build-l bowtie2-build-s bowtie2-inspect bowtie2-inspect-l bowtie2-inspect-s
WORKDIR /tmp
#   install samtools
RUN apt-get install --no-install-recommends libncurses-dev libbz2-dev=1.0.6-8ubuntu0.2 -y
RUN wget https://github.com/samtools/samtools/releases/download/1.5/samtools-1.5.tar.bz2
RUN tar -jxf samtools-1.5.tar.bz2
WORKDIR /tmp/samtools-1.5
RUN ./configure --disable-lzma
RUN make -j 16
RUN mv samtools /usr/local/bin/
WORKDIR /tmp
#   Compile and install PriceSeqFilter
RUN wget http://derisilab.ucsf.edu/software/price/PriceSource140408.tar.gz
RUN tar -xzf PriceSource140408.tar.gz
WORKDIR /tmp/PriceSource140408
RUN make -j 16
RUN mv PriceSeqFilter /usr/local/bin/
WORKDIR /tmp
#   Compile and install cdhit-dup tools
RUN wget https://github.com/weizhongli/cdhit/archive/V4.6.8.zip
RUN unzip V4.6.8.zip
WORKDIR /tmp/cdhit-4.6.8
RUN make -j 16
WORKDIR /tmp/cdhit-4.6.8/cd-hit-auxtools
RUN make -j 16
RUN mv cd-hit-dup /usr/local/bin/
WORKDIR /tmp
#   Compile and install Fastax tools
#  RUN wget http://launchpadlibrarian.net/161878011/libgtextutils0_0.7-1_amd64.deb
#  RUN wget http://launchpadlibrarian.net/162265652/fastx-toolkit_0.0.14-1_amd64.deb
#  RUN gdebi --non-interactive libgtextutils0_0.7-1_amd64.deb
#  RUN gdebi --non-interactive fastx-toolkit_0.0.14-1_amd64.deb
#   For aegea
RUN apt-get install --no-install-recommends python3-pip=8.1.1-2ubuntu0.6 -y
RUN pip3 install awscli-cwlogs==1.4.0 keymaker==0.2.1 boto3==1.4.3 awscli==1.11.44 dynamoq==0.0.5 tractorbeam==0.1.3
RUN pip3 install pysam biopython
#  RUN echo iptables-persistent iptables-persistent/autosave_v4 boolean true | debconf-set-selections
RUN apt-get update \
 && apt-get install --no-install-recommends iptables-persistent=1.0.4 debian-goodies=0.64 bridge-utils=1.5-9ubuntu1 pixz=1.0.6-2 cryptsetup-bin=2:1.6.6-5ubuntu2.1 mdadm=3.3-2ubuntu7.6 btrfs-tools=4.4-1ubuntu1.1 libffi-dev=3.2.1-4 libssl-dev=1.0.2g-1ubuntu4.20 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxslt1-dev=1.1.28-2.1ubuntu0.3 libyaml-dev=0.1.6-3 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 libjemalloc-dev=3.6.0-9ubuntu1 libzip-dev=1.0.1-0ubuntu1 libsnappy-dev=1.1.3-2 liblz4-dev=0.0~r131-2ubuntu2 libgmp-dev=2:6.1.0+dfsg-2 libmpfr-dev=3.1.4-1 libhts-dev=1.2.1-2ubuntu1 libsqlite3-dev=3.11.0-1ubuntu1.5 libncurses5-dev=6.0+20160213-1ubuntu1 htop=2.0.1-1ubuntu1 pydf=12 jq=1.5+dfsg-1ubuntu0.1 httpie=0.9.2-1 python-dev=2.7.12-1~16.04 python-cffi=1.5.2-1ubuntu1 python-pip=8.1.1-2ubuntu0.6 python-setuptools=20.7.0-1 python-wheel=0.29.0-1 python-virtualenv=15.0.1+ds-3ubuntu1.1 python-requests=2.9.1-3ubuntu0.1 python-yaml=3.11-3build1 python3-dev=3.5.1-3 python3-cffi=1.5.2-1ubuntu1 python3-pip=8.1.1-2ubuntu0.6 python3-setuptools=20.7.0-1 python3-wheel=0.29.0-1 python3-requests=2.9.1-3ubuntu0.1 python3-yaml=3.11-3build1 nfs-common=1:1.2.8-9ubuntu12.3 unzip=6.0-20ubuntu1.1 build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 libtool=2.4.6-0.1 autoconf=2.69-9 ruby=1:2.3.0+1 sysstat=11.2.0-1ubuntu0.3 dstat=0.7.2-4 numactl=2.0.11-1ubuntu1.1 gdebi-core=0.9.5.7ubuntu1 sqlite3=3.11.0-1ubuntu1.5 stunnel moreutils=0.57-1 curl=7.47.0-1ubuntu2.19 wget=1.17.1-1ubuntu1.5 git=1:2.7.4-0ubuntu1.10 aria2=1.19.0-1build1 sift=4.0.3b-4 -y
#   Strange that we have to do this, but if we don't, aegea tries to do it, and it fails then with some urllib3 bug.
RUN apt-get install --no-install-recommends awscli=1.18.69-1ubuntu0.16.04.1 -y
RUN apt-get install --no-install-recommends bsdtar=3.1.2-11ubuntu0.16.04.8 alien=8.95 -y
#   For de-novo assembly
WORKDIR /tmp/spades_build
RUN git clone https://github.com/ablab/spades.git
WORKDIR /tmp/spades_build/spades
RUN git checkout spades_3.11.0
WORKDIR /tmp/spades_build/spades/assembler
RUN PREFIX=/usr/local ./spades_compile.sh
RUN /usr/local/bin/spades.py --test
#   For nonhost fastq filtering
WORKDIR /tmp/seqtk_build
RUN git clone https://github.com/lh3/seqtk.git
WORKDIR /tmp/seqtk_build/seqtk
RUN make \
 && make install
WORKDIR /tmp
#   Compile and install gmap/gsnap
RUN wget http://research-pub.gene.com/gmap/src/gmap-gsnap-2017-11-15.tar.gz
RUN mkdir gmap-gsnap \
 && tar xf gmap-gsnap-2017-11-15.tar.gz -C gmap-gsnap --strip-components 1
WORKDIR /tmp/gmap-gsnap
RUN ./configure --prefix=/usr/local
RUN make -j 16 \
 && make check \
 && make install
RUN rm -rf /tmp/gmap-gsnap /tmp/gmap-gsnap-2017-11-15.tar.gz
RUN gsnapl --version
#   For srst2, install forked srst2 python 3 compatible repo
RUN pip install scipy==1.10.1
RUN pip install git+https://github.com/chanzuckerberg/srst2
#   TODO: Test both pip installations, consider keeping pip use consistent
RUN pip3 install pandas
#   Blast command line
RUN wget -N ftp://ftp.ncbi.nlm.nih.gov/blast/executables/blast+/2.6.0/ncbi-blast-2.6.0+-1.x86_64.rpm
RUN alien -i ncbi-blast-2.6.0+-1.x86_64.rpm
RUN rm -rf ncbi-blast-2.6.0+-1.x86_64.rpm
#   For adapter trimming
RUN apt-get install --no-install-recommends python-cutadapt=1.9.1-1build1
WORKDIR /tmp
RUN wget http://www.usadellab.org/cms/uploads/supplementary/Trimmomatic/Trimmomatic-0.38.zip
RUN unzip Trimmomatic-0.38.zip
RUN mv Trimmomatic-0.38/trimmomatic-0.38.jar /usr/local/bin/
RUN apt-get update \
 && apt-get install --no-install-recommends default-jre=2:1.8-56ubuntu2 -y
#   For phylogenetic trees
WORKDIR /tmp
RUN wget https://sourceforge.net/projects/ksnp/files/kSNP3.1_Linux_package.zip
RUN unzip -o kSNP3.1_Linux_package.zip
WORKDIR /tmp/kSNP3.1_Linux_package/kSNP3
RUN cp -r * /usr/local/bin/
RUN sed -i 's:set kSNP=/usr/local/kSNP3:set kSNP=/usr/local/bin:g' /usr/local/bin/kSNP3
RUN apt-get install --no-install-recommends tcsh=6.18.01-5
RUN kSNP3
RUN apt-get install --no-install-recommends liblz4-tool=0.0~r131-2ubuntu2 -y
RUN apt-get install --no-install-recommends lbzip2=2.5-1 -y
#   Cleanup
RUN rm -rf /tmp/*
WORKDIR /
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
