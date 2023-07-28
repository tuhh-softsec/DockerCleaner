FROM sbfnk/libbi-gpu
LABEL mantainer="Sebastian Funk <sebastian.funk@lshtm.ac.uk>"
ARG R_VERSION
ARG BUILD_DATE
ENV R_VERSION="${R_VERSION:-3.5.1}" \
    LC_ALL="en_US.UTF-8" \
    LANG="en_US.UTF-8" \
    TERM="xterm"
RUN apt-get update \
 && apt-get install --no-install-recommends bash-completion=1:2.11-6ubuntu1 file=1:5.44-3 fonts-texgyre=20180621-6 g++=4:12.2.0-3ubuntu1 gfortran=4:12.2.0-3ubuntu1 gsfonts=2:20200910-7 libblas-dev=3.11.0-2 libbz2-1.0=1.0.8-5build1 libcurl3 libicu55 libjpeg-turbo8=2.1.5-2ubuntu1 libopenblas-dev=0.3.21+ds-4 libpangocairo-1.0-0=1.50.12+ds-1 libpcre3=2:8.39-15 libpng16-16=1.6.39-2 libreadline6 libtiff5=4.4.0-6ubuntu1 liblzma5=5.4.1-0.2 locales=2.37-0ubuntu2 make=4.3-4.1build1 unzip=6.0-27ubuntu1 zip=3.0-13 zlib1g=1:1.2.13.dfsg-1ubuntu4 -y \
 && echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
 && locale-gen en_US.utf8 \
 && /usr/sbin/update-locale LANG=en_US.UTF-8 \
 && BUILDDEPS="curl default-jdk libbz2-dev libcairo2-dev libcurl4-openssl-dev libpango1.0-dev libpcre3-dev libpng-dev libreadline-dev libtiff5-dev liblzma-dev libx11-dev libxt-dev tcl8.6-dev tk8.6-dev texinfo texlive-extra-utils texlive-fonts-recommended texlive-fonts-extra texlive-latex-recommended x11proto-core-dev xauth xfonts-base xvfb" \
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
 && Rscript -e "install.packages(c('littler', 'docopt'))" \
 && ln -s /usr/local/lib/R/site-library/littler/examples/install2.r /usr/local/bin/install2.r \
 && ln -s /usr/local/lib/R/site-library/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
 && ln -s /usr/local/lib/R/site-library/littler/bin/r /usr/local/bin/r \
 && cd / \
 && rm -rf /tmp/* \
 && apt-get remove --purge -y $BUILDDEPS \
 && apt-get autoremove -y \
 && apt-get autoclean -y \
 && rm -rf /var/lib/apt/lists/*
#   install R packages
RUN install2.r -e data.table ggplot2 lubridate scales coda GGally ggmcmc remotes ncdf4 \
 && rm -rf /tmp/downloaded_packages
RUN installGithub.r sbfnk/rbi sbfnk/rbi.helpers \
 && rm -rf /tmp/downloaded_packages
CMD ["R"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
