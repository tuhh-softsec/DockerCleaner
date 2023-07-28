FROM ubuntu:18.04
MAINTAINER bhaas@broadinstitute.org
RUN apt-get update \
 && apt-get install --no-install-recommends gcc=4:7.4.0-1ubuntu2.3 g++=4:7.4.0-1ubuntu2.3 perl=5.26.1-6ubuntu0.6 python3=3.6.7-1~18.04 automake=1:1.15.1-3ubuntu2 make=4.1-9.1ubuntu1 wget=1.19.4-1ubuntu2.2 curl=7.58.0-2ubuntu3.24 libdb-dev=1:5.3.21~exp1ubuntu2 bzip2=1.0.6-8.1ubuntu0.2 zlibc=0.9k-4.3 zlib1g=1:1.2.11.dfsg-0ubuntu2.2 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 default-jre=2:1.11-68ubuntu1~18.04.1 python-setuptools=39.0.1-2ubuntu0.1 python-dev=2.7.15~rc1-1 build-essential=12.4ubuntu1 unzip=6.0-21ubuntu1.2 libbz2-dev=1.0.6-8.1ubuntu0.2 liblzma-dev=5.2.2-1.3ubuntu0.1 -y \
 && apt-get clean
RUN ln -sf /usr/bin/python3 /usr/bin/python
RUN apt-get install --no-install-recommends python3-distutils=3.6.9-1~18.04 -y
RUN curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py \
 && python get-pip.py
RUN pip install numpy==1.24.2 requests==2.28.2 igv-reports==1.7.0
RUN curl -L https://cpanmin.us | perl - App::cpanminus
#  # set up tool config and deployment area:
ENV SRC="/usr/local/src"
ENV BIN="/usr/local/bin"
ENV DATA="/usr/local/data"
RUN mkdir $DATA
#  # perl lib installations
RUN cpanm install PerlIO::gzip
RUN cpanm install Set::IntervalTree
RUN cpanm install DB_File
RUN cpanm install URI::Escape
RUN cpanm install Carp::Assert
RUN cpanm install JSON::XS.pm
#  #######
#   Samtools
ENV SAMTOOLS_VERSION="1.7"
RUN SAMTOOLS_URL="https://github.com/samtools/samtools/releases/download/${SAMTOOLS_VERSION}/samtools-${SAMTOOLS_VERSION}.tar.bz2" \
 && cd $SRC \
 && wget $SAMTOOLS_URL \
 && tar xvf samtools-${SAMTOOLS_VERSION}.tar.bz2 \
 && cd samtools-${SAMTOOLS_VERSION}/htslib-${SAMTOOLS_VERSION} \
 && ./configure \
 && make \
 && make install \
 && cd ../ \
 && ./configure --without-curses \
 && make \
 && make install
#  #######
#   Trinity
RUN apt-get install --no-install-recommends cmake=3.10.2-1ubuntu2.18.04.2 -y
ENV TRINITY_VERSION="2.8.4"
RUN TRINITY_URL="https://github.com/trinityrnaseq/trinityrnaseq/archive/Trinity-v${TRINITY_VERSION}.tar.gz" \
 && cd $SRC \
 && wget $TRINITY_URL \
 && tar xvf Trinity-v${TRINITY_VERSION}.tar.gz \
 && cd trinityrnaseq-Trinity-v${TRINITY_VERSION} \
 && make
ENV TRINITY_HOME="/usr/local/src/trinityrnaseq-Trinity-v${TRINITY_VERSION}"
#  # Bowtie2
WORKDIR $SRC
RUN wget https://sourceforge.net/projects/bowtie-bio/files/bowtie2/2.3.3.1/bowtie2-2.3.3.1-linux-x86_64.zip/download -O bowtie2-2.3.3.1-linux-x86_64.zip \
 && unzip bowtie2-2.3.3.1-linux-x86_64.zip \
 && mv bowtie2-2.3.3.1-linux-x86_64/bowtie2* $BIN \
 && rm *.zip \
 && rm -r bowtie2-2.3.3.1-linux-x86_64
#  # Jellyfish
WORKDIR $SRC
RUN wget https://github.com/gmarcais/Jellyfish/releases/download/v2.2.7/jellyfish-2.2.7.tar.gz \
 && tar xvf jellyfish-2.2.7.tar.gz \
 && cd jellyfish-2.2.7/ \
 && ./configure \
 && make \
 && make install
#  # Salmon
WORKDIR $SRC
RUN wget https://github.com/COMBINE-lab/salmon/releases/download/v0.9.1/Salmon-0.9.1_linux_x86_64.tar.gz \
 && tar xvf Salmon-0.9.1_linux_x86_64.tar.gz \
 && ln -s $SRC/Salmon-latest_linux_x86_64/bin/salmon $BIN/.
#  #############
#  # STAR
ENV STAR_VERSION="2.7.0f"
RUN STAR_URL="https://github.com/alexdobin/STAR/archive/${STAR_VERSION}.tar.gz" \
 && wget -P $SRC $STAR_URL \
 && tar -xvf $SRC/${STAR_VERSION}.tar.gz -C $SRC \
 && mv $SRC/STAR-${STAR_VERSION}/bin/Linux_x86_64_static/STAR /usr/local/bin
#  #######
#   GMAP  (compile this last because this version unfortunately disrupts some headers in DL_LIBRARY_PATH)
ENV GMAP_VERSION="2017-11-15"
WORKDIR $SRC
RUN GMAP_URL="http://research-pub.gene.com/gmap/src/gmap-gsnap-$GMAP_VERSION.tar.gz" \
 && wget $GMAP_URL \
 && tar xvf gmap-gsnap-$GMAP_VERSION.tar.gz \
 && cd gmap-$GMAP_VERSION \
 && ./configure \
 && make \
 && make install
RUN apt-get clean \
 && apt-get update \
 && apt-get install --no-install-recommends locales=2.27-3ubuntu1.6 -y
RUN locale-gen en_US.UTF-8
#  ##############
#  # STAR-Fusion:
WORKDIR $SRC
ENV STAR_FUSION_VERSION="1.6.0"
ENV STARF_CHECKOUT="c465cf33d111121f6be127483379bebd2f866fbd"
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.17.1-1ubuntu0.17 -y
RUN git clone https://github.com/STAR-Fusion/STAR-Fusion.git \
 && cd STAR-Fusion \
 && git checkout $STARF_CHECKOUT \
 && git submodule init \
 && git submodule update \
 && cd FusionInspector \
 && git submodule init \
 && git submodule update
ENV STAR_FUSION_HOME="$SRC/STAR-Fusion"
#  # FusionInspector and FusionAnnotator are bundled with STAR-Fusion
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
