FROM debian:stretch
#  # From the Build-Depends of the Debian R package, plus subversion
RUN apt-get update -qq \
 && apt-get install --no-install-recommends ca-certificates=20200601~deb9u2 curl=7.52.1-5+deb9u16 ed=1.10-2.1 fonts-texgyre=20160520-1 less=481-2.1 locales=2.24-11+deb9u4 libreadline-dev=7.0-3 vim-tiny=2:8.0.0197-4+deb9u7 wget=1.18-5+deb9u3 -y \
 && echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
 && locale-gen en_US.utf8 \
 && /usr/sbin/update-locale LANG=en_US.UTF-8 \
 && apt-get install --no-install-recommends bash-completion=1:2.1-4.3 bison=2:3.0.4.dfsg-1+b1 debhelper=10.2.5 default-jdk=2:1.8-58+deb9u1 g++=4:6.3.0-4 gcc=4:6.3.0-4 gfortran=4:6.3.0-4 groff-base=1.22.3-9 libblas-dev=3.7.0-2 libbz2-dev=1.0.6-8.1 libcairo2-dev=1.14.8-1+deb9u1 libcurl4-openssl-dev=7.52.1-5+deb9u16 libjpeg-dev=1:1.5.1-2+deb9u2 liblapack-dev=3.7.0-2 liblzma-dev=5.2.2-1.2+deb9u1 libncurses5-dev=6.0+20161126-1+deb9u2 libpango1.0-dev=1.40.5-1 libpcre3-dev=2:8.39-3 libpng-dev=1.6.28-1+deb9u1 libreadline-dev=7.0-3 libtiff5-dev=4.0.8-2+deb9u8 libx11-dev=2:1.6.4-3+deb9u4 libxt-dev=1:1.1.5-1 mpack=1.6-8.1 rsync=3.1.2-1+deb9u3 subversion=1.9.5-1+deb9u6 tcl8.6-dev=8.6.6+dfsg-1+b1 texinfo=6.3.0.dfsg.1-1+b2 texlive-base=2016.20170123-5 texlive-extra-utils=2016.20170123-5 texlive-fonts-extra=2016.20170123-5 texlive-fonts-recommended=2016.20170123-5 texlive-generic-recommended=2016.20170123-5 texlive-latex-base=2016.20170123-5 texlive-latex-extra=2016.20170123-5 texlive-latex-recommended=2016.20170123-5 tk8.6-dev=8.6.6-1+b1 valgrind=1:3.12.0~svn20160714-1+b1 x11proto-core-dev=7.0.31-1 xauth=1:1.0.9-1+b2 xdg-utils=1.1.1-1+deb9u2 xfonts-base=1:1.0.4+nmu1 xvfb=2:1.19.2-1+deb9u9 zlib1g-dev=1:1.2.8.dfsg-5+deb9u1 -y \
 && cd /tmp \
 && svn co https://svn.r-project.org/R/trunk R-devel \
 && cd /tmp/R-devel \
 && ./tools/rsync-recommended \
 && R_PAPERSIZE=letter R_BATCHSAVE="--no-save --no-restore" R_BROWSER=xdg-open PAGER=/usr/bin/pager PERL=/usr/bin/perl R_UNZIPCMD=/usr/bin/unzip R_ZIPCMD=/usr/bin/zip R_PRINTCMD=/usr/bin/lpr LIBnn=lib AWK=/usr/bin/awk CFLAGS="-pipe -Wall -pedantic -O2 -mtune=native -fsanitize=address" FFLAGS="-pipe -O2 -mtune=native" FCFLAGS="-pipe -O2 -mtune=native" CXXFLAGS="-pipe -Wall -pedantic -O2 -mtune=native" MAIN_LDFLAGS="-fsanitize=address,undefined" CC="gcc -std=gnu99 -fsanitize=address,undefined -fno-omit-frame-pointer" CXX="g++ -fsanitize=address,undefined,bounds-strict -fno-omit-frame-pointer -fno-sanitize=object-size,vptr" CXX1X="g++ -fsanitize=address,undefined,bounds-strict -fno-omit-frame-pointer -fno-sanitize=object-size,vptr" FC="gfortran" F77="gfortran" ./configure --enable-R-shlib --with-blas --with-readline --with-recommended-packages --program-suffix=dev --disable-openmp \
 && make \
 && make install \
 && make clean \
 && echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site \
 && mkdir -p /usr/local/lib/R/site-library \
 && chown root:staff /usr/local/lib/R/site-library \
 && chmod g+wx /usr/local/lib/R/site-library \
 && echo "R_LIBS_USER='/usr/local/lib/R/site-library'" >> /usr/local/lib/R/etc/Renviron \
 && echo "R_LIBS=${R_LIBS-'/usr/local/lib/R/site-library:/usr/local/lib/R/library:/usr/lib/R/library'}" >> /usr/local/lib/R/etc/Renviron \
 && [ -z "$BUILD_DATE" ] \
 && BUILD_DATE=$( TZ="America/Los_Angeles" date -I ;) || true \
 && MRAN=https://mran.microsoft.com/snapshot/${BUILD_DATE} \
 && echo MRAN=$MRAN >> /etc/environment \
 && export MRAN=$MRAN \
 && Rscript -e "install.packages(c('littler', 'docopt'), repos = '$MRAN')" \
 && ln -s /usr/local/lib/R/site-library/littler/examples/install2.r /usr/local/bin/install2.r \
 && ln -s /usr/local/lib/R/site-library/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
 && ln -s /usr/local/lib/R/site-library/littler/bin/r /usr/local/bin/r \
 && curl -O /usr/local/bin/install2.r https://github.com/eddelbuettel/littler/raw/master/inst/examples/install2.r \
 && chmod +x /usr/local/bin/install2.r \
 && cd / \
 && rm -rf /tmp/* \
 && apt-get remove --purge -y $BUILDDEPS \
 && apt-get autoremove -y \
 && apt-get autoclean -y \
 && rm -rf /var/lib/apt/lists/*
ENV LC_ALL="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
#  # More verbose UBSAN/SAN output (cf #3) -- this is still somewhat speculative
#  # Entry copied from Prof Ripley's setup described at http://www.stats.ox.ac.uk/pub/bdr/memtests/README.txt
ENV ASAN_OPTIONS="'alloc_dealloc_mismatch=0:detect_leaks=0:detect_odr_violation=0'"
CMD ["R"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
