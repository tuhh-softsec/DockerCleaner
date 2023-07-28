FROM debian:jessie
LABEL org.label-schema.license="GPL-2.0" \
      org.label-schema.vcs-url="https://github.com/rocker-org/rocker-versioned" \
      org.label-schema.vendor="Rocker Project" \
      maintainer="Carl Boettiger <cboettig@ropensci.org>"
ARG R_VERSION
ARG BUILD_DATE
ENV BUILD_DATE="${BUILD_DATE:-2015-06-18}"
ENV R_VERSION="${R_VERSION:-3.2.0}" \
    LC_ALL="en_US.UTF-8" \
    LANG="en_US.UTF-8" \
    TERM="xterm"
# # dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends bash-completion ca-certificates curl file fonts-texgyre g++ gfortran gsfonts libblas-dev libbz2-1.0 libcurl3 libicu52 libjpeg62-turbo libopenblas-dev libpangocairo-1.0-0 libpcre3 libpng12-0 libtiff5 liblzma5 locales make unzip zip zlib1g -y \
 && echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
 && locale-gen en_US.utf8 \
 && /usr/sbin/update-locale LANG=en_US.UTF-8 \
 && BUILDDEPS=" default-jdk libbz2-dev libcairo2-dev libcurl4-openssl-dev libpango1.0-dev libjpeg-dev libicu-dev libpcre3-dev libpng-dev libreadline-dev libtiff5-dev liblzma-dev libx11-dev libxt-dev perl tcl8.5-dev tk8.5-dev texinfo texlive-extra-utils texlive-fonts-recommended texlive-fonts-extra texlive-latex-recommended x11proto-core-dev xauth xfonts-base xvfb zlib1g-dev" \
 && apt-get install --no-install-recommends $BUILDDEPS -y \
 && cd tmp/ \
 && curl -O https://cran.r-project.org/src/base/R-3/R-${R_VERSION}.tar.gz \
 && tar -xf R-${R_VERSION}.tar.gz \
 && cd R-${R_VERSION} \
 && R_PAPERSIZE=letter R_BATCHSAVE="--no-save --no-restore" R_BROWSER=xdg-open PAGER=/usr/bin/pager PERL=/usr/bin/perl R_UNZIPCMD=/usr/bin/unzip R_ZIPCMD=/usr/bin/zip R_PRINTCMD=/usr/bin/lpr LIBnn=lib AWK=/usr/bin/awk CFLAGS="-g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g" CXXFLAGS="-g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g" ./configure --enable-R-shlib --enable-memory-profiling --with-readline --with-blas --with-tcltk --disable-nls --with-recommended-packages \
 && make \
 && make install \
 && echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'curl')" >> /usr/local/lib/R/etc/Rprofile.site \
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
 && echo "options(repos = c(CRAN='$MRAN'), download.file.method = 'curl')" >> /usr/local/lib/R/etc/Rprofile.site \
 && Rscript -e "install.packages(c('littler', 'docopt'), repo = '$MRAN')" \
 && ln -s /usr/local/lib/R/site-library/littler/examples/install2.r /usr/local/bin/install2.r \
 && ln -s /usr/local/lib/R/site-library/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
 && ln -s /usr/local/lib/R/site-library/littler/bin/r /usr/local/bin/r \
 && cd / \
 && rm -rf /tmp/* \
 && apt-get remove --purge -y $BUILDDEPS \
 && apt-get autoremove -y \
 && apt-get autoclean -y \
 && rm -rf /var/lib/apt/lists/*
CMD ["R"]
