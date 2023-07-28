FROM ubuntu
MAINTAINER Sung-Huan Yu <sung-huan.yu@uni-wuerzburg.de>
ENV DEBIAN_FRONTEND="noninteractive"
RUN :
RUN apt-get upgrade --yes
RUN (apt-get update ;apt-get install --no-install-recommends default-jre=2:1.17-74 default-jdk=2:1.17-74 python3=3.11.2-1 python3-setuptools=66.1.1-1 python3-scipy=1.10.1-1 vim=2:9.0.1000-4ubuntu2 make=4.3-4.1build1 gcc=4:12.2.0-3ubuntu1 g++=4:12.2.0-3ubuntu1 gfortran=4:12.2.0-3ubuntu1 libx11-dev=2:1.8.4-2 wget=1.21.3-1ubuntu1 zip=3.0-13 unzip=6.0-27ubuntu1 python3-biopython=1.80+dfsg-4build1 cython3=0.29.32-2ubuntu2 software-properties-common=0.99.35 python3-software-properties=0.99.35 bioperl=1.7.8-1 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 ncbi-blast+=2.12.0+ds-3build1 pkg-config=1.8.1-1ubuntu2 python3-dev=3.11.2-1 libfreetype6-dev=2.12.1+dfsg-4 libxft-dev=2.3.6-1 r-base=4.2.2.20221110-2build1 libxml2-dev=2.9.14+dfsg-1.1build2 libpng-dev=1.6.39-2 python3-pip=23.0.1+dfsg-1 python-pip python3-numpy=1:1.24.2-1 imagemagick=8:6.9.11.60+dfsg-1.6 infernal=1.1.4-1 git=1:2.39.2-1ubuntu1 openssh-client=1:9.0p1-1ubuntu8 apache2=2.4.55-1ubuntu2 curl=7.88.1-7ubuntu1 build-essential=12.9ubuntu3 net-tools=2.10-0.1ubuntu3 librpc-xml-perl=0.82-1 ncbi-blast+-legacy=2.12.0+ds-3build1 nano=7.2-1 libf2c2=20140711-1 apache2-dev=2.4.55-1ubuntu2 libapache-singleton-perl=0.17-2 libjson-rpc-perl=1.06-3 libncurses5-dev=6.4-2 build-essential=12.9ubuntu3 hmmer=3.3.2+dfsg-1 lua5.1=5.1.5-9 blast2 snap=2013-11-29-11 cpanminus=1.7046-1 mummer=3.23+dfsg-8 exonerate=2.4.0-5 mafft=7.505-1 fasttree=2.1.11-2 libsvg-perl=2.87-1 libboost-all-dev=1.74.0.3ubuntu7 libgd-svg-perl=0.33-3 python-setuptools libc6-i386=2.37-0ubuntu2 lib32stdc++6=13-20230320-1ubuntu1 lib32gcc1 netcat genometools=1.6.2+ds-3 last-align=1447-1 libboost-iostreams-dev=1.74.0.3ubuntu7 libgsl-dev=2.7.1+dfsg-3 liblpsolve55-dev=5.5.2.5-2build2 libstdc++6=13-20230320-1ubuntu1 aragorn=1.2.38-4 tantan=40-1 libstorable-perl libbio-perl-perl=1.7.8-1 libsqlite3-dev=3.40.1-1 tree=2.1.0-1 --yes --fix-missing )
RUN ln -fs /usr/bin/fasttree /usr/bin/FastTree
RUN pip3 install matplotlib networkx READemption ANNOgesic
RUN mkdir /tools /data
WORKDIR /tools
#   vienna package
RUN wget http://www.tbi.univie.ac.at/RNA/packages/source/ViennaRNA-2.3.2.tar.gz \
 && tar -zxvf ViennaRNA-2.3.2.tar.gz \
 && cd ViennaRNA-2.3.2 \
 && ./configure --without-perl --without-python \
 && make \
 && make install \
 && cp src/Utils/relplot.pl /usr/local/bin \
 && cp src/Utils/mountain.pl /usr/local/bin
#   IntaRNA
RUN wget https://github.com/BackofenLab/IntaRNA/releases/download/v2.0.4/intaRNA-2.0.4.tar.gz \
 && tar -zxvf intaRNA-2.0.4.tar.gz \
 && cd intaRNA-2.0.4 \
 && ./configure \
 && make \
 && make install
#   sratoolkit
RUN wget http://ftp-trace.ncbi.nlm.nih.gov/sra/sdk/2.5.2/sratoolkit.2.5.2-ubuntu64.tar.gz \
 && tar -zxvf sratoolkit.2.5.2-ubuntu64.tar.gz \
 && rm sratoolkit.2.5.2-ubuntu64.tar.gz
#   RATT
RUN wget ftp://ftp.sanger.ac.uk/pub/resources/software/pagit/PAGIT.V1.64bit.tgz \
 && tar -zxvf PAGIT.V1.64bit.tgz \
 && mv PAGIT/RATT /opt/RATT \
 && rm -rf PAGIT
#   patch the error of perl version and the path of mummer
RUN sed -i '244s/defined//' /opt/RATT/main.ratt.pl \
 && sed -i '19s/$PAGIT_HOME/\/usr/' /opt/RATT/start.ratt.sh
ENV RATT_HOME="/opt/RATT" \
    PERL5LIB="/opt/RATT/:$PERL5LIB" \
    PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/RATT:$PATH"
#   TSSpredator
RUN wget https://lambda.informatik.uni-tuebingen.de/nexus/content/repositories/releases/org/uni-tuebingen/it/TSSpredator/1.06/TSSpredator-1.06.jar \
 && cp TSSpredator-1.06.jar /usr/local/bin/TSSpredator.jar
#   MEME
RUN wget http://www.mpich.org/static/downloads/3.2/mpich-3.2.tar.gz \
 && tar -zxvf mpich-3.2.tar.gz \
 && cd mpich-3.2 \
 && ./configure \
 && make all install
RUN wget http://meme-suite.org/meme-software/4.11.1/meme_4.11.1.tar.gz \
 && tar -zxvf meme_4.11.1.tar.gz
RUN perl -MCPAN -e 'install HTML::Template; install XML::Compile::SOAP11; install XML::Compile::WSDL11; install XML::Compile::Transport::SOAPHTTP; install XML::RPC'
RUN cpan install HTML::Template \
 && cpan HTML::PullParser \
 && cpan XML::Simple \
 && cpan XML::Compile::WSDL11 \
 && cpan XML::Compile::SOAP11
RUN cd meme_4.11.1 \
 && ./configure --prefix=/tools/meme --with-url=http://meme.nbcr.net/meme --enable-build-libxml2 --enable-build-libxslt \
 && make \
 && make test \
 && make install \
 && cp /tools/meme/bin/* /usr/local/bin
#  # htslib, samtools, bcftools
RUN wget https://github.com/samtools/htslib/releases/download/1.3.1/htslib-1.3.1.tar.bz2
RUN tar -jxvf htslib-1.3.1.tar.bz2 \
 && cd htslib-1.3.1 \
 && ./configure \
 && make \
 && make install \
 && cd ..
RUN wget https://github.com/samtools/samtools/releases/download/1.3.1/samtools-1.3.1.tar.bz2
RUN tar -jxvf samtools-1.3.1.tar.bz2 \
 && cd samtools-1.3.1 \
 && ./configure \
 && make \
 && make install \
 && cd ..
RUN wget https://github.com/samtools/bcftools/releases/download/1.3.1/bcftools-1.3.1.tar.bz2
RUN tar -jxvf bcftools-1.3.1.tar.bz2 \
 && cd bcftools-1.3.1 \
 && make \
 && make install \
 && cd ..
#   segemehl TODO:update to latest version. Some parameters are changed
RUN wget http://www.bioinf.uni-leipzig.de/Software/segemehl/old/segemehl_0_2_0.tar.gz \
 && tar -zxvf segemehl_0_2_0.tar.gz \
 && cd segemehl_0_2_0/segemehl \
 && make all \
 && cp *.x /usr/local/bin
#   transtermHP
RUN wget http://transterm.cbcb.umd.edu/transterm_hp_v2.09.zip \
 && unzip transterm_hp_v2.09.zip \
 && cd transterm_hp_v2.09 \
 && cp transterm /usr/local/bin \
 && cp expterm.dat /usr/local/bin
#   CRT
RUN wget http://www.room220.com/crt/CRT1.2-CLI.jar.zip \
 && unzip CRT1.2-CLI.jar.zip \
 && cp CRT1.2-CLI.jar /usr/local/bin/CRT.jar
#   Psortb
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends supervisor=4.2.1-1ubuntu1 -y ) \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
ENV APACHE_RUN_USER="www-data" \
    APACHE_RUN_GROUP="www-data" \
    APACHE_LOG_DIR="/var/log/apache2" \
    APACHE_LOCK_DIR="/var/lock/apache2" \
    APACHE_PID_FILE="/var/run/apache2.pid"
WORKDIR /usr/local/src
RUN echo '/usr/local/lib64' >> /etc/ld.so.conf \
 && wget http://www.psort.org/download/docker/pft2.3.4.docker64bit.tar.gz \
 && tar zxvf pft2.3.4.docker64bit.tar.gz \
 && cp pftools/pfscan /usr/local/bin/
RUN wget http://www.psort.org/download/libpsortb-1.0.tar.gz \
 && tar zxvf libpsortb-1.0.tar.gz \
 && cd libpsortb-1.0 \
 && ./configure \
 && make \
 && make install \
 && ldconfig
RUN wget http://www.psort.org/download/bio-tools-psort-all.3.0.4.tar.gz \
 && tar zxvf bio-tools-psort-all.3.0.4.tar.gz
WORKDIR /usr/local/src/bio-tools-psort-all
RUN wget http://www.psort.org/download/docker/psortb.defaults \
 && perl Makefile.PL \
 && make \
 && make install \
 && cp -r psort /usr/local/psortb
RUN a2enmod cgid \
 && wget http://www.psort.org/download/docker/apache.conf.fragment \
 && cat apache.conf.fragment >> /etc/apache2/apache2.conf
WORKDIR /usr/local/src
RUN wget http://www.psort.org/download/docker/apache-svm.tar.gz \
 && tar zxvf apache-svm.tar.gz \
 && cd apache-svm \
 && make \
 && cp svmloc.conf /etc/apache2/conf-available/
RUN wget http://www.psort.org/download/docker/startup.txt \
 && mv startup.txt startup.pl \
 && wget http://www.psort.org/download/docker/apache-psort.conf \
 && cp apache-psort.conf /etc/apache2/conf-available/
RUN wget http://www.psort.org/download/docker/apache-psortb.tar.gz \
 && tar zxvf apache-psortb.tar.gz \
 && cd apache-psortb \
 && perl Makefile.PL \
 && make \
 && make install
RUN cd /etc/apache2/conf-enabled/ \
 && ln -s ../conf-available/svmloc.conf \
 && ln -s ../conf-available/apache-psort.conf
RUN wget http://www.psort.org/download/docker/Request.pm \
 && cp Request.pm /usr/share/perl5/Apache/Singleton/Request.pm
RUN wget http://www.psort.org/download/docker/CGI-FastTemplate-1.09.tar.gz \
 && tar zxvf CGI-FastTemplate-1.09.tar.gz \
 && cd CGI-FastTemplate-1.09 \
 && perl Makefile.PL \
 && make \
 && make install
RUN cd /var/www/html \
 && wget http://www.psort.org/download/docker/psort-web.tar.gz \
 && tar zxvf psort-web.tar.gz
RUN rm -r pft2.3.4.docker64bit.tar.gz libpsortb-1.0.tar.gz libpsortb-1.0 bio-tools-psort-all.3.0.4.tar.gz bio-tools-psort-all
RUN /etc/init.d/apache2 restart
EXPOSE 80/tcp
CMD ["/opt/run.sh"]
#   copy psort to global execute
RUN cp /usr/local/psortb/bin/psort /usr/local/bin
WORKDIR /tools
#   sometimes the pypi does not update to latest version.
#   may need to re-write the code
RUN pip3 install ANNOgesic --upgrade --force-reinstall
RUN rm segemehl_0_2_0.tar.gz meme_4.11.1.tar.gz transterm_hp_v2.09.zip htslib-1.3.1.tar.bz2 bcftools-1.3.1.tar.bz2 samtools-1.3.1.tar.bz2 ViennaRNA-2.3.2.tar.gz intaRNA-2.0.4.tar.gz CRT1.2-CLI.jar.zip PAGIT.V1.64bit.tgz
#   a patch for language setting, need to re-write the code.
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends locales=2.37-0ubuntu2 -y )
RUN locale-gen en_US.UTF-8
WORKDIR /data
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
