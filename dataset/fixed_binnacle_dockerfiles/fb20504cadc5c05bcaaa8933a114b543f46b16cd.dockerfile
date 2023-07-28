FROM ubuntu:16.04
MAINTAINER dekkerlab
USER root
#    a lot of it is stollen from :
#   https://hub.docker.com/r/genomicpariscentre/bioperl/dockerfile/
#   install a bunch of system-wide stuff
#   and figure out later what's essential ...
RUN apt-get update --fix-missing \
 && apt-get install --no-install-recommends apt-utils=1.2.35 wget=1.17.1-1ubuntu1.5 curl=7.47.0-1ubuntu2.19 bzip2=1.0.6-8ubuntu0.2 pkg-config=0.29.1-0ubuntu1 libbz2-dev=1.0.6-8ubuntu0.2 git=1:2.7.4-0ubuntu1.10 build-essential=12.1ubuntu2 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 locales=2.23-0ubuntu11.3 vim=2:7.4.1689-3ubuntu1.5 fontconfig=2.11.94-0ubuntu1.1 perl=5.22.1-9ubuntu0.9 expat=2.1.0-7ubuntu0.16.04.5 libexpat-dev ttf-dejavu=2.35-1 cpanminus=1.7040-1 libgd-perl=2.53-2.1 -q -y
#   Set the locale
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
#   Install perl modules 
RUN apt-get install --no-install-recommends cpanminus=1.7040-1 -y
RUN cpanm CPAN::Meta readline Term::ReadKey YAML Digest::SHA Module::Build ExtUtils::MakeMaker Test::More Data::Stag Config::Simple Statistics::Lite Statistics::Descriptive
RUN apt-get install --no-install-recommends libarchive-zip-perl=1.56-2ubuntu0.1 --yes
#   Install GD
RUN apt-get remove --yes libgd-gd2-perl
RUN apt-get install --no-install-recommends libgd2-noxpm-dev --yes
RUN cpanm GD GD::Graph GD::Graph::smoothlines
#   install imagemagick
RUN apt-get install --no-install-recommends imagemagick=8:6.8.9.9-7ubuntu5.16 --yes
#   Install conda
RUN curl -LO http://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh \
 && bash Miniconda3-latest-Linux-x86_64.sh -p /miniconda3 -b \
 && rm Miniconda3-latest-Linux-x86_64.sh
ENV PATH="/miniconda3/bin:${PATH}"
#   Install conda dependencies
COPY cworld_environment.yml /
COPY VERSION /
RUN pwd
RUN conda config --set always_yes yes --set changeps1 no \
 && conda config --add channels conda-forge \
 && conda config --add channels defaults \
 && conda config --add channels bioconda \
 && conda config --get \
 && conda update -q conda \
 && conda info -a \
 && conda env update -q -n root --file cworld_environment.yml \
 && conda clean --tarballs --index-cache --lock
RUN conda install pysam
#  export MKL OMP etc ...
#   # get the version of the GDlib:
#   RUN perl -MGD -e 'print $GD::VERSION ."\n";'
#   # local isntall
#   perl Build.PL
#   ./Build
#   ./Build install --install_base /your/custom/dir
#   (ensure /your/custom/dir is added to your PERL5LIB path)
#   e.g.
#   ./Build install --install_base ~/perl5
#   # then in .bashrc
#   export PERL5LIB=${PERL5LIB}:/home/<yourusername>/perl5/lib/perl5
#  RUN git clone https://github.com/dekkerlab/cworld-dekker.git
WORKDIR /cworld-dekker
COPY Build.PL .
COPY lib ./lib
COPY scripts ./scripts
COPY MANIFEST .
RUN pwd
RUN ls -lah
#   global install ...
RUN perl ./Build.PL
RUN ./Build
RUN ./Build install
RUN ./Build install --install_base /perl5
ENV PERL5LIB="${PERL5LIB}:/perl5/lib/perl5"
RUN ./Build distclean
#   After installing the module, you should be free to run anything in scripts/ e.g.
#   $ perl scripts/heatmap.pl
#   WORKDIR /root
#   it should be more civilized, but for now, let's hope it just works
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
