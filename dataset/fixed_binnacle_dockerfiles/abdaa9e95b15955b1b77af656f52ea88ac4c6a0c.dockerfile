FROM debian
MAINTAINER bhaas@broadinstitute.org
RUN apt-get update \
 && apt-get install --no-install-recommends gcc=4:12.2.0-3ubuntu1 g++=4:12.2.0-3ubuntu1 perl=5.36.0-7 python automake=1:1.16.5-1.3 make=4.3-4.1build1 wget=1.21.3-1ubuntu1 git=1:2.39.2-1ubuntu1 curl=7.88.1-7ubuntu1 libdb-dev=1:5.3.21~exp1ubuntu4 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 bzip2=1.0.8-5build1 libncurses5-dev=6.4-2 texlive-latex-base=2022.20230122-2 openjdk-7-jre python-pip python-dev -y \
 && apt-get clean
RUN curl -L https://cpanmin.us | perl - App::cpanminus
RUN cpanm install DB_File
RUN cpanm install Set::IntervalTree
RUN cpanm install URI::Escape
RUN pip install pysam==0.21.0
#  # set up tool config and deployment area:
ENV SRC="/usr/local/src"
ENV BIN="/usr/local/bin"
ENV DATA="/usr/local/data"
RUN mkdir $DATA
#  #####################
#  # Tool installations:
#  #####################
#  ##############
#  # STAR-Fusion:
RUN cd $SRC \
 && git clone --recursive https://github.com/STAR-Fusion/STAR-Fusion.git
ENV STAR_FUSION_HOME="$SRC/STAR-Fusion"
#  #############
#  # STAR
RUN RELEASE="2.5.2a" \
 && STAR_URL="https://github.com/alexdobin/STAR/archive/${RELEASE}.tar.gz" \
 && wget -P $SRC $STAR_URL \
 && tar -xvf $SRC/${RELEASE}.tar.gz -C $SRC \
 && mv $SRC/STAR-${RELEASE}/bin/Linux_x86_64_static/STAR /usr/local/bin
#  ##################
#  # FusionInspector
RUN cd $SRC \
 && git clone --recursive https://github.com/FusionInspector/FusionInspector.git
ENV FUSION_INSPECTOR_HOME="$SRC/FusionInspector"
#  #########
#  # Trinity
RUN TRINITY_URL="https://github.com/trinityrnaseq/trinityrnaseq/archive/v2.1.1.tar.gz" \
 && wget -P $SRC $TRINITY_URL \
 && tar -xvf $SRC/v2.1.1.tar.gz -C $SRC \
 && cd $SRC/trinityrnaseq-2.1.1 \
 && make
ENV TRINITY_HOME="$SRC/trinityrnaseq-2.1.1"
RUN cp $TRINITY_HOME/trinity-plugins/htslib/bgzip $BIN
RUN cp $TRINITY_HOME/trinity-plugins/BIN/samtools $BIN
RUN cp $TRINITY_HOME/trinity-plugins/htslib/tabix $BIN
#  ############
#  # Oases
RUN VELVET_URL="http://www.ebi.ac.uk/~zerbino/velvet/velvet_1.2.10.tgz" \
 && wget -P $SRC $VELVET_URL \
 && tar xvf $SRC/velvet_1.2.10.tgz -C $SRC \
 && ln -s $SRC/velvet_1.2.10 $SRC/velvet \
 && cd $SRC/velvet \
 && make \
 && cp velveth velvetg $BIN/
RUN OASES_URL="https://www.ebi.ac.uk/~zerbino/oases/oases_0.2.08.tgz" \
 && wget -P $SRC $OASES_URL \
 && tar -xvf $SRC/oases_0.2.08.tgz -C $SRC \
 && cd $SRC/oases_0.2.8 \
 && make \
 && cp oases $BIN/
#  #############
#  # DISCASM
RUN cd $SRC \
 && git clone --recursive https://github.com/DISCASM/DISCASM.git
ENV DISCASM_HOME="$SRC/DISCASM"
#  ##############################
#  # Install
COPY PerlLib $SRC/
ENV PERL5LIB="$SRC:${PERL5LIB}"
COPY util/*.pl $BIN/
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
