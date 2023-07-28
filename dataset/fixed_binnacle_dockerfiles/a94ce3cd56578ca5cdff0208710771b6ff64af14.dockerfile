#  #
#  # BUILD image for the ChIPseq pipeline
#  # AVAILABLE COMMANDS:
#  #    build: 
#  #      $ docker build -t imbforge/chipseq:v1 .
#  #    push to the docker hub: 
#  #      $ docker push imbforge/chipseq:v1
#  #    run a container: 
#  #      $ docker run --rm -v ${WORKDIR}:${WORKDIR} -w ${WORKDIR} -t imbforge/chipseq:v1 \
#  #         -n ${MAX_PARALLEL_PROCS} ${WORKDIR}/chipseq_v1.2.txt ${WORKDIR}/rawdata/*.fastq.gz
#  #    run an interactive shell:
#  #      $ docker run --entrypoint=/bin/bash -ti imbforge/chipseq:v1 -s
#  #
#  FROM ubuntu:14.04
FROM debian:stable
MAINTAINER Sergi Sayols <s.sayolspuig@imb-mainz.de>
ENTRYPOINT ["/opt/bpipe/default/bin/bpipe", "run"]
#  # env vars
ENV HOME="/home"
ENV DEBIAN_FRONTEND="noninteractive"
ENV TOOL_DEPENDENCIES="/opt"
#  # create user to avoid running the thing as root
#   Note: Users and groups in an image get a non-deterministic UID/GID in that the
#     "next" UID/GID gets assigned regardless of image rebuilds. So, if it’s critical,
#     you should assign an explicit UID/GID.
#  RUN groupadd -r imbcf && \ 
#  	useradd -r -g imbcf -s /bin/bash -c "Docker image user" imbcf
#  # install all deps in a single RUN statement to save space
RUN apt-get update \
 && apt-get dist-upgrade -y \
 && apt-get install --no-install-recommends apt-utils=2.2.4 -y \
 && apt-get install --no-install-recommends build-essential=12.9 gcc=4:10.2.1-1 g++=4:10.2.1-1 gfortran=4:10.2.1-1 make=4.3-4.1 -y \
 && apt-get install --no-install-recommends libboost-dev=1.74.0.3 -y \
 && apt-get install --no-install-recommends xorg-dev=1:7.7+22 libglu1-mesa-dev=9.0.1-1 libpango1.0-dev=1.46.2-3 -y \
 && apt-get install --no-install-recommends libxml2-dev=2.9.10+dfsg-6.7+deb11u3 libcurl4-openssl-dev=7.74.0-1.3+deb11u7 -y \
 && apt-get install --no-install-recommends openjdk-7-jre-headless -y \
 && apt-get install --no-install-recommends python2.7-dev=2.7.18-8 python-numpy python-matplotlib python-setuptools=44.1.1-1 -y \
 && apt-get install --no-install-recommends zlib1g-dev=1:1.2.11.dfsg-2+deb11u2 libncurses5-dev=6.2+20201114-2 -y \
 && apt-get install --no-install-recommends bc=1.07.1-2+b2 -y \
 && apt-get clean autoclean \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#  # install R and the needed packages
COPY ./deps/R-3.2.2 /usr/local/src/R-3.2.2
WORKDIR /usr/local/src/R-3.2.2
RUN ./configure --prefix=/opt/R/3.2.2 --with-readline=no \
 && make \
 && make install \
 && /opt/R/3.2.2/bin/Rscript -e 'source("http://bioconductor.org/biocLite.R"); biocLite(c("GenomicAlignments","GenomicRanges","Rsamtools","ShortRead"),ask=FALSE)' \
 && /opt/R/3.2.2/bin/Rscript -e 'install.packages(c("snow","knitr"),dependencies=TRUE,repos="http://cran.rstudio.org")' \
 && rm -rf /usr/local/src/R-3.2.2 \
 && ln -s /opt/R/3.2.2 /opt/R/latest \
 && ln -s /opt/R/3.2.2 /opt/R/default \
 && echo "export PATH=/opt/R/3.2.2/bin:$PATH" > /opt/R/3.2.2/env.sh \
 && chmod ugo+rx /opt/R/3.2.2/env.sh
#  # install SPP
COPY ./deps/spp_1.11.tar.gz /usr/local/src/
WORKDIR /usr/local/src
RUN /opt/R/3.2.2/bin/R CMD INSTALL spp_1.11.tar.gz \
 && rm spp_1.11.tar.gz
#  # install FastQC
COPY ./deps/FastQC /opt/fastqc/0.11.3
RUN ln -s /opt/fastqc/0.11.3 /opt/fastqc/latest \
 && ln -s /opt/fastqc/0.11.3 /opt/fastqc/default \
 && echo "export PATH=/opt/fastqc/0.11.3:$PATH" > /opt/fastqc/0.11.3/env.sh \
 && chmod ugo+rx /opt/fastqc/0.11.3/env.sh
#  # install bowtie
COPY ./deps/bowtie-1.1.1 /opt/bowtie/1.1.1
RUN ln -s /opt/bowtie/1.1.1 /opt/bowtie/latest \
 && ln -s /opt/bowtie/1.1.1 /opt/bowtie/default \
 && echo "export PATH=/opt/bowtie/1.1.1:$PATH" > /opt/bowtie/1.1.1/env.sh \
 && chmod ugo+rx /opt/bowtie/1.1.1/env.sh
#  # install samtools
COPY ./deps/samtools-1.2 /usr/local/src/samtools-1.2
WORKDIR /usr/local/src/samtools-1.2
RUN make \
 && make install prefix=/opt/samtools/1.2 \
 && rm -rf /usr/local/src/samtools-1.2 \
 && ln -s /opt/samtools/1.2 /opt/samtools/latest \
 && ln -s /opt/samtools/1.2 /opt/samtools/default \
 && echo "export PATH=/opt/samtools/1.2/bin:$PATH" > /opt/samtools/1.2/env.sh \
 && chmod ugo+rx /opt/samtools/1.2/env.sh
#  # install Picard tools
COPY ./deps/picard-tools-1.123 /opt/picard/1.123
RUN ln -s /opt/picard/1.123 /opt/picard/latest \
 && ln -s /opt/picard/1.123 /opt/picard/default \
 && echo "export PATH=/opt/picard/1.123:$PATH" > /opt/picard/1.123/env.sh \
 && chmod ugo+rx /opt/picard/1.123/env.sh
#  # install bedtools
COPY ./deps/bedtools2-2.25.0 /usr/local/src/bedtools2-2.25.0
WORKDIR /usr/local/src/bedtools2-2.25.0
RUN make \
 && make install prefix=/opt/BEDTools/2.25.0 \
 && rm -rf /usr/local/src/bedtools2-2.25.0 \
 && ln -s /opt/BEDTools/2.25.0 /opt/BEDTools/latest \
 && ln -s /opt/BEDTools/2.25.0 /opt/BEDTools/default \
 && echo "export PATH=/opt/BEDTools/2.25.0/bin:$PATH" > /opt/BEDTools/2.25.0/env.sh \
 && chmod ugo+rx /opt/BEDTools/2.25.0/env.sh
#  # install UCSC tools
COPY ./deps/ucsc-2014-02-05 /opt/ucsc/2014-02-05
RUN ln -s /opt/ucsc/2014-02-05 /opt/ucsc/latest \
 && ln -s /opt/ucsc/2014-02-05 /opt/ucsc/default \
 && echo "export PATH=/opt/ucsc/2014-02-05/:$PATH" > /opt/ucsc/2014-02-05/env.sh \
 && chmod ugo+rx /opt/ucsc/2014-02-05/env.sh
#  # install macs2
COPY ./deps/macs2-2.1.0 /usr/local/src/macs2-2.1.0
WORKDIR /usr/local/src/macs2-2.1.0
RUN mkdir -p /opt/macs2/2.1.0/lib/python2.7/site-packages \
 && export PYTHONPATH=/opt/macs2/2.1.0/lib/python2.7/site-packages:$PYTHONPATH \
 && python setup.py build \
 && python setup.py install --prefix=/opt/macs2/2.1.0 \
 && rm -rf /usr/local/src/macs2-2.1.0 \
 && ln -s /opt/macs2/2.1.0 /opt/macs2/latest \
 && ln -s /opt/macs2/2.1.0 /opt/macs2/default \
 && echo "export PATH=/opt/macs2/2.1.0/bin:$PATH" > /opt/macs2/2.1.0/env.sh \
 && echo "export PYTHONPATH=/opt/macs2/2.1.0/lib/python2.7/site-packages:$PYTHONPATH" >> /opt/macs2/2.1.0/env.sh \
 && chmod ugo+rx /opt/macs2/2.1.0/env.sh
#  # install bpipe
COPY ./deps/bpipe-0.9.8.7 /opt/bpipe/0.9.8.7
RUN ln -s /opt/bpipe/0.9.8.7 /opt/bpipe/latest \
 && ln -s /opt/bpipe/0.9.8.7 /opt/bpipe/default \
 && echo "export PATH=${TOOL_DEPENDENCIES}/bpipe/0.9.8.7/bin/:$PATH" > /opt/bpipe/0.9.8.7/env.sh \
 && chmod ugo+rx /opt/bpipe/0.9.8.7/env.sh
#  # install pipelines and wrappers
COPY ./deps/imb-forge /opt/imb-forge
RUN ln -s /opt/imb-forge/pipelines/chipseq/1.2 /opt/imb-forge/pipelines/chipseq/latest \
 && ln -s /opt/imb-forge/pipelines/chipseq/1.2 /opt/imb-forge/pipelines/chipseq/default
#  # post install tasks
WORKDIR /
RUN rm -rf /usr/local/src/*
#  USER imbcf
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
