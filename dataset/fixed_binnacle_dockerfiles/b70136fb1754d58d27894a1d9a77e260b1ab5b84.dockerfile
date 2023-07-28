#  ##################################################
#   Stage 1 - docker container to build ensembl-vep #
#  ##################################################
FROM ubuntu:18.04 AS builder
#   Update aptitude and install some required packages
#   a lot of them are required for Bio::DB::BigFile
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 git=1:2.17.1-1ubuntu0.17 libpng-dev=1.6.34-1ubuntu0.18.04.2 perl=5.26.1-6ubuntu0.6 perl-base=5.26.1-6ubuntu0.6 unzip=6.0-21ubuntu1.2 wget=1.19.4-1ubuntu2.2 -y \
 && rm -rf /var/lib/apt/lists/*
#   Setup VEP environment
ENV OPT="/opt/vep"
ENV OPT_SRC="$OPT/src"
ENV HTSLIB_DIR="$OPT_SRC/htslib"
#   Branch to clone, e.g. "-b release/96"
ENV BRANCH="\"
#   Working directory
WORKDIR $OPT_SRC
#   Clone/download repositories/libraries
#   Clone ensembl git repository and extract useful ensemb core file
RUN git clone $BRANCH --depth 1 https://github.com/Ensembl/ensembl.git \
 && cp ensembl/cpanfile ensembl_cpanfile \
 && rm -rf ensembl \
 && git clone $BRANCH --depth 1 https://github.com/Ensembl/ensembl-vep.git \
 && chmod u+x ensembl-vep/*.pl \
 && git clone $BRANCH --depth 1 https://github.com/Ensembl/ensembl-variation.git \
 && mkdir var_c_code \
 && cp ensembl-variation/C_code/*.c ensembl-variation/C_code/Makefile var_c_code/ \
 && rm -rf ensembl-variation \
 && chmod u+x var_c_code/* \
 && git clone --depth 1 https://github.com/bioperl/bioperl-ext.git \
 && wget https://github.com/Ensembl/ensembl-xs/archive/2.3.2.zip -O ensembl-xs.zip \
 && unzip -q ensembl-xs.zip \
 && mv ensembl-xs-2.3.2 ensembl-xs \
 && rm -rf ensembl-xs.zip \
 && ensembl-vep/travisci/get_dependencies.sh \
 && mv bioperl-live bioperl-live_bak \
 && mkdir bioperl-live \
 && mv bioperl-live_bak/Bio bioperl-live/ \
 && rm -rf bioperl-live_bak \
 && rm -rf Bio-HTS/.??* Bio-HTS/Changes Bio-HTS/DISCLAIMER Bio-HTS/MANIFEST* Bio-HTS/README Bio-HTS/scripts Bio-HTS/t Bio-HTS/travisci bioperl-ext/.??* bioperl-ext/Bio/SeqIO bioperl-ext/Bio/Tools bioperl-ext/Makefile.PL bioperl-ext/README* bioperl-ext/t bioperl-ext/examples ensembl-vep/.??* ensembl-vep/docker ensembl-xs/.??* ensembl-xs/Changes ensembl-xs/INSTALL ensembl-xs/MANIFEST ensembl-xs/README ensembl-xs/t ensembl-xs/travisci htslib/.??* htslib/INSTALL htslib/NEWS htslib/README* htslib/test \
 && mv kent-335_base kent-335_base_bak \
 && mkdir -p kent-335_base/src \
 && cp -R kent-335_base_bak/confs kent-335_base/ \
 && cp -R kent-335_base_bak/src/lib kent-335_base_bak/src/inc kent-335_base_bak/src/jkOwnLib kent-335_base/src/ \
 && cp kent-335_base_bak/src/*.sh kent-335_base/src/ \
 && rm -rf kent-335_base_bak
#   Setup bioperl-ext
WORKDIR bioperl-ext/Bio/Ext/Align/
RUN perl -pi -e"s|(cd libs.+)CFLAGS=\'|$1CFLAGS=\'-fPIC |" Makefile.PL
#   Install htslib binaries (need bgzip, tabix)
WORKDIR $HTSLIB_DIR
RUN make install \
 && rm -f Makefile *.c cram/*.c
#   Compile Variation LD C scripts
WORKDIR $OPT_SRC/var_c_code
RUN make \
 && rm -f Makefile *.c
#  ##################################################
#   Stage 2 - docker container to build ensembl-vep #
#  ##################################################
FROM ubuntu:18.04
#   Update aptitude and install some required packages
#   a lot of them are required for Bio::DB::BigFile
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 cpanminus=1.7043-1 curl=7.58.0-2ubuntu3.24 libmysqlclient-dev=5.7.41-0ubuntu0.18.04.1 libpng-dev=1.6.34-1ubuntu0.18.04.2 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 locales=2.27-3ubuntu1.6 openssl=1.1.1-1ubuntu2.1~18.04.21 perl=5.26.1-6ubuntu0.6 perl-base=5.26.1-6ubuntu0.6 unzip=6.0-21ubuntu1.2 vim=2:8.0.1453-1ubuntu1.11 -y \
 && apt-get -y purge manpages-dev \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Setup VEP environment
ENV OPT="/opt/vep"
ENV OPT_SRC="$OPT/src"
ENV PERL5LIB_TMP="$PERL5LIB:$OPT_SRC/ensembl-vep:$OPT_SRC/ensembl-vep/modules"
ENV PERL5LIB="$PERL5LIB_TMP:$OPT_SRC/bioperl-live"
ENV KENT_SRC="$OPT/src/kent-335_base/src"
ENV HTSLIB_DIR="$OPT_SRC/htslib"
ENV MACHTYPE="x86_64"
ENV DEPS="$OPT_SRC"
ENV PATH="$OPT_SRC/ensembl-vep:$OPT_SRC/var_c_code:$PATH"
ENV LANG_VAR="en_US.UTF-8"
#   Create vep user
RUN useradd -r -m -U -d "$OPT" -s /bin/bash -c "VEP User" -p '' vep \
 && usermod -a -G sudo vep \
 && mkdir -p $OPT_SRC
USER vep
#   Copy downloaded libraries (stage 1) to this image (stage 2)
COPY --chown=vep:vep --from=builder $OPT_SRC $OPT_SRC
#  ############################################################
#   Change user to root for the following complilations/installations
USER root
#   Install bioperl-ext, faster alignments for haplo (XS-based BioPerl extensions to C libraries)
WORKDIR $OPT_SRC/bioperl-ext/Bio/Ext/Align/
RUN perl Makefile.PL \
 && make \
 && make install \
 && rm -f Makefile*
#   Install ensembl-xs, faster run using re-implementation in C of some of the Perl subroutines
WORKDIR $OPT_SRC/ensembl-xs
RUN perl Makefile.PL \
 && make \
 && make install \
 && rm -f Makefile*
WORKDIR $OPT_SRC
#   Install/compile more libraries
RUN ensembl-vep/travisci/build_c.sh \
 && cpanm --installdeps --with-recommends --notest --cpanfile ensembl_cpanfile . \
 && cpanm --installdeps --with-recommends --notest --cpanfile ensembl-vep/cpanfile . \
 && rm -rf bioperl-live \
 && echo "$LANG_VAR UTF-8" >> /etc/locale.gen \
 && locale-gen en_US.utf8 \
 && /usr/sbin/update-locale LANG=$LANG_VAR \
 && cp $HTSLIB_DIR/bgzip $HTSLIB_DIR/tabix $HTSLIB_DIR/htsfile /usr/local/bin/
ENV LC_ALL="$LANG_VAR"
ENV LANG="$LANG_VAR"
#   Switch back to vep user
USER vep
ENV PERL5LIB="$PERL5LIB_TMP"
#   Final steps
WORKDIR $OPT_SRC/ensembl-vep
#   Update bash profile
RUN echo >> $OPT/.profile \
 && echo PATH=$PATH:$PATH >> $OPT/.profile \
 && echo export PATH >> $OPT/.profile \
 && ./INSTALL.pl -a ap -g miRNA,LoF -l \
 && rm -rf t travisci .travis.yml
WORKDIR /
COPY loftee.tgz $OPT/src/ensembl-vep/modules
USER root
RUN apt-get update \
 && apt-get install --no-install-recommends python3-pip=9.0.1-2.3~ubuntu1.18.04.8 python3-dev=3.6.7-1~18.04 -y \
 && cd /usr/local/bin \
 && ln -s /usr/bin/python3 python \
 && pip3 install --upgrade pip
RUN apt-get update \
 && apt-get install --no-install-recommends apache2=2.4.29-1ubuntu4.27 apt-utils=1.6.14 build-essential=12.4ubuntu1 cpanminus=1.7043-1 curl=7.58.0-2ubuntu3.24 git=1:2.17.1-1ubuntu0.17 libmysqlclient-dev=5.7.41-0ubuntu0.18.04.1 libpng-dev=1.6.34-1ubuntu0.18.04.2 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 manpages=4.15-1 mysql-client=5.7.41-0ubuntu0.18.04.1 openssl=1.1.1-1ubuntu2.1~18.04.21 perl=5.26.1-6ubuntu0.6 perl-base=5.26.1-6ubuntu0.6 unzip=6.0-21ubuntu1.2 vim=2:8.0.1453-1ubuntu1.11 wget=1.19.4-1ubuntu2.2 sudo=1.8.21p2-3ubuntu1.5 -y
#   install ensembl dependencies
RUN cpanm Test::Object PPI::Document Task::Weaken Test::SubCalls Test::Object DBI DBD::mysql Archive::Zip Perl::Critic Set::IntervalTree
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=1.6.14
RUN sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN sudo apt-get update \
 && sudo apt-get -y install software-properties-common
RUN sudo add-apt-repository 'deb [arch=amd64,i386] https://cran.rstudio.com/bin/linux/ubuntu xenial/'
USER root
WORKDIR /
ENV PACKAGE_BIO="libhts2 bedtools"
ENV PACKAGE_DEV="gfortran gcc-multilib autoconf liblzma-dev libncurses5-dev libblas-dev liblapack-dev libssh2-1-dev libxml2-dev vim libssl-dev libcairo2-dev libbz2-dev libcurl4-openssl-dev"
ENV PYTHON_MODULES="numpy cython scipy pandas cyvcf2 toml"
RUN apt-get update \
 && apt-get install --no-install-recommends nano=2.9.3-2 ed=1.10-2.1 locales=2.27-3ubuntu1.6 vim-tiny=2:8.0.1453-1ubuntu1.11 fonts-texgyre=20160520-1 $PACKAGE_DEV $PACKAGE_BIO -y \
 && rm -rf /var/lib/apt/lists/*
RUN apt-get autoremove
#  # Install vcfanno version v0.3.1
RUN wget https://github.com/brentp/vcfanno/releases/download/v0.3.1/vcfanno_linux64 \
 && mv vcfanno_linux64 vcfanno \
 && mv vcfanno /usr/local/bin \
 && chmod 755 /usr/local/bin/vcfanno
#  # Install Ensembl's Vcf-validator
RUN wget https://github.com/EBIvariation/vcf-validator/releases/download/v0.6/vcf_validator \
 && mv vcf_validator /usr/local/bin/ \
 && chmod 755 /usr/local/bin/vcf_validator
USER root
WORKDIR /
RUN wget https://github.com/samtools/samtools/releases/download/1.9/samtools-1.9.tar.bz2
RUN bunzip2 -dc samtools-1.9.tar.bz2 | tar xvf -
RUN cd samtools-1.9 \
 && ./configure --prefix=/usr/local/bin \
 && make \
 && make install
WORKDIR /
#  # Install tools used for compilation
RUN sudo -H pip install --upgrade pip
RUN sudo -H pip install -U setuptools
RUN sudo -H pip install $PYTHON_MODULES
#
RUN wget http://ab-initio.mit.edu/nlopt/nlopt-2.4.2.tar.gz \
 && gzip -dc nlopt-2.4.2.tar.gz | tar xvf - \
 && cd nlopt-2.4.2 \
 && ./configure \
 && make \
 && make install
RUN apt-get update \
 && apt-get install --no-install-recommends libpq-dev=10.23-0ubuntu0.18.04.1 libxt-dev=1:1.1.5-1 libudunits2-dev=2.2.26-1 -y
USER root
WORKDIR /
RUN git clone https://github.com/atks/vt.git
WORKDIR vt
RUN make
RUN make test
RUN cp vt /usr/local/bin
#  RUN ln -s /usr/bin/python3.6 /usr/local/bin/python
RUN export PATH=/usr/local/bin:$PATH
#  # Clean Up
RUN apt-get clean autoclean
RUN rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN rm -rf /var/lib/{dpkg,cache,log}
VOLUME /workdir
WORKDIR /workdir/
USER root
RUN mkdir /data \
 && chmod 777 /data
WORKDIR /data
VOLUME /data
USER root
WORKDIR /
RUN rm -f nlopt-2.4.2.tar.gz
RUN rm -rf $HOME/src/ensembl-vep/t/
RUN rm -f $HOME/src/v335_base.tar.gz
RUN rm -f $HOME/src/release-1-6-924.zip
RUN rm -rf /vt
RUN rm -rf /samtools-1.9.tar.bz2
COPY gvanno.tgz /
ENV PATH="$PATH:/gvanno"
ENV PYTHONPATH=":/gvanno/lib:${PYTHONPATH}"
#  ENV VCFANNO_DATA_DOCKER="/data"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
