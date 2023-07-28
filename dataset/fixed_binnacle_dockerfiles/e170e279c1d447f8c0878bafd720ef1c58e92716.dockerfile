FROM debian:stretch
LABEL org.label-schema.license="GPL-2.0" \
      org.label-schema.vcs-url="https://github.com/rocker-org/rocker-versioned" \
      org.label-schema.vendor="Rocker Project" \
      maintainer="Carl Boettiger <cboettig@ropensci.org>"
ARG R_VERSION
ARG BUILD_DATE
ENV BUILD_DATE="${BUILD_DATE:-2017-06-30}"
ENV R_VERSION="${R_VERSION:-3.4.0}" \
    LC_ALL="en_US.UTF-8" \
    LANG="en_US.UTF-8" \
    TERM="xterm"
RUN apt-get update \
 && apt-get install --no-install-recommends bash-completion=1:2.1-4.3 ca-certificates=20200601~deb9u2 file=1:5.30-1+deb9u3 fonts-texgyre=20160520-1 g++=4:6.3.0-4 gfortran=4:6.3.0-4 gsfonts=1:8.11+urwcyr1.0.7~pre44-4.3 libblas-dev=3.7.0-2 libbz2-1.0=1.0.6-8.1 libcurl3=7.52.1-5+deb9u16 libicu57=57.1-6+deb9u5 libjpeg62-turbo=1:1.5.1-2+deb9u2 libopenblas-dev=0.2.19-3 libpangocairo-1.0-0=1.40.5-1 libpcre3=2:8.39-3 libpng16-16=1.6.28-1+deb9u1 libreadline7=7.0-3 libtiff5=4.0.8-2+deb9u8 liblzma5=5.2.2-1.2+deb9u1 locales=2.24-11+deb9u4 make=4.1-9.1 unzip=6.0-21+deb9u2 zip=3.0-11+b1 zlib1g=1:1.2.8.dfsg-5+deb9u1 -y \
 && echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
 && locale-gen en_US.utf8 \
 && /usr/sbin/update-locale LANG=en_US.UTF-8 \
 && BUILDDEPS="curl default-jdk libbz2-dev libcairo2-dev libcurl4-openssl-dev libpango1.0-dev libjpeg-dev libicu-dev libpcre3-dev libpng-dev libreadline-dev libtiff5-dev liblzma-dev libx11-dev libxt-dev perl tcl8.6-dev tk8.6-dev texinfo texlive-extra-utils texlive-fonts-recommended texlive-fonts-extra texlive-latex-recommended x11proto-core-dev xauth xfonts-base xvfb zlib1g-dev" \
 && apt-get install --no-install-recommends $BUILDDEPS -y \
 && cd tmp/ \
 && curl -O https://cran.r-project.org/src/base/R-3/R-${R_VERSION}.tar.gz \
 && tar -xf R-${R_VERSION}.tar.gz \
 && cd R-${R_VERSION} \
 && R_PAPERSIZE=letter R_BATCHSAVE="--no-save --no-restore" R_BROWSER=xdg-open PAGER=/usr/bin/pager PERL=/usr/bin/perl R_UNZIPCMD=/usr/bin/unzip R_ZIPCMD=/usr/bin/zip R_PRINTCMD=/usr/bin/lpr LIBnn=lib AWK=/usr/bin/awk CFLAGS="-g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g" CXXFLAGS="-g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g" ./configure --enable-R-shlib --enable-memory-profiling --with-readline --with-blas --with-tcltk --disable-nls --with-recommended-packages \
 && make \
 && make install \
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
 && echo "options(repos = c(CRAN='$MRAN'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site \
 && Rscript -e "install.packages(c('littler', 'docopt'), repo = '$MRAN')" \
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
CMD ["R"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
