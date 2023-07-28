# ##################################################
#  Stage 1 - docker container to build ensembl-vep #
# ##################################################
FROM ubuntu:18.04 AS builder
#  Update aptitude and install some required packages
#  a lot of them are required for Bio::DB::BigFile
RUN apt-get update \
 && apt-get install build-essential git libpng-dev perl perl-base unzip wget -y \
 && rm -rf /var/lib/apt/lists/*
#  Setup VEP environment
ENV OPT="/opt/vep"
ENV OPT_SRC="$OPT/src"
ENV HTSLIB_DIR="$OPT_SRC/htslib"
ENV BRANCH="release/96"
#  Working directory
WORKDIR $OPT_SRC
#  Clone/download repositories/libraries
#  Clone ensembl git repository and extract useful ensemb core file
RUN git clone --branch $BRANCH --depth 1 https://github.com/Ensembl/ensembl.git \
 && cp ensembl/cpanfile ensembl_cpanfile \
 && rm -rf ensembl \
 && git clone --branch $BRANCH --depth 1 https://github.com/Ensembl/ensembl-vep.git \
 && chmod u+x ensembl-vep/*.pl \
 && git clone --branch $BRANCH --depth 1 https://github.com/Ensembl/ensembl-variation.git \
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
#  Setup bioperl-ext
WORKDIR bioperl-ext/Bio/Ext/Align/
RUN perl -pi -e"s|(cd libs.+)CFLAGS=\\'|$1CFLAGS=\\'-fPIC |" Makefile.PL
#  Install htslib binaries (need bgzip, tabix)
WORKDIR $HTSLIB_DIR
RUN make install \
 && rm -f Makefile *.c cram/*.c
#  Compile Variation LD C scripts
WORKDIR $OPT_SRC/var_c_code
RUN make \
 && rm -f Makefile *.c
# ##################################################
#  Stage 2 - docker container to build ensembl-vep #
# ##################################################
FROM ubuntu:18.04
#  Update aptitude and install some required packages
#  a lot of them are required for Bio::DB::BigFile
RUN apt-get update \
 && apt-get install build-essential cpanminus curl libmysqlclient-dev libpng-dev libssl-dev locales openssl perl perl-base unzip vim -y \
 && apt-get -y purge manpages-dev \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  Setup VEP environment
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
#  Create vep user
RUN useradd -r -m -U -d "$OPT" -s /bin/bash -c "VEP User" -p '' vep \
 && usermod -a -G sudo vep \
 && mkdir -p $OPT_SRC
USER vep
#  Copy downloaded libraries (stage 1) to this image (stage 2)
COPY --chown=vep:vep --from=builder $OPT_SRC $OPT_SRC
# ############################################################
#  Change user to root for the following complilations/installations
USER root
#  Install bioperl-ext, faster alignments for haplo (XS-based BioPerl extensions to C libraries)
WORKDIR $OPT_SRC/bioperl-ext/Bio/Ext/Align/
RUN perl Makefile.PL \
 && make \
 && make install \
 && rm -f Makefile*
#  Install ensembl-xs, faster run using re-implementation in C of some of the Perl subroutines
WORKDIR $OPT_SRC/ensembl-xs
RUN perl Makefile.PL \
 && make \
 && make install \
 && rm -f Makefile*
WORKDIR $OPT_SRC
#  Install/compile more libraries
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
#  Switch back to vep user
USER vep
ENV PERL5LIB="$PERL5LIB_TMP"
#  Final steps
WORKDIR $OPT_SRC/ensembl-vep
#  Update bash profile
RUN echo >> $OPT/.profile \
 && echo PATH=$PATH:$PATH >> $OPT/.profile \
 && echo export PATH >> $OPT/.profile \
 && ./INSTALL.pl -a a -l \
 && rm -rf t travisci .travis.yml
