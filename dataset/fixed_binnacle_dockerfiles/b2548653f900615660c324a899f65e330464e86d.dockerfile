FROM rocker/r-ver:3.5.2
ARG RSTUDIO_VERSION
ENV RSTUDIO_VERSION="${RSTUDIO_VERSION:-1.1.463}"
ARG S6_VERSION
ARG PANDOC_TEMPLATES_VERSION
ENV S6_VERSION="${S6_VERSION:-v1.21.7.0}"
ENV S6_BEHAVIOUR_IF_STAGE2_FAILS="2"
ENV PATH="/usr/lib/rstudio-server/bin:$PATH"
ENV PANDOC_TEMPLATES_VERSION="${PANDOC_TEMPLATES_VERSION:-2.6}"
#  # Download and install RStudio server & dependencies
#  # Attempts to get detect latest version, otherwise falls back to version given in $VER
#  # Symlink pandoc, pandoc-citeproc so they are available system-wide
RUN apt-get update \
 && apt-get install --no-install-recommends file git libapparmor1 libcurl4-openssl-dev libedit2 libssl-dev lsb-release psmisc procps python-setuptools sudo wget -y \
 && wget -O libssl1.0.0.deb http://ftp.debian.org/debian/pool/main/o/openssl/libssl1.0.0_1.0.1t-1+deb8u8_amd64.deb \
 && dpkg -i libssl1.0.0.deb \
 && rm libssl1.0.0.deb \
 && RSTUDIO_LATEST=$( wget --no-check-certificate -qO- https://s3.amazonaws.com/rstudio-server/current.ver ;) \
 && [ -z "$RSTUDIO_VERSION" ] \
 && RSTUDIO_VERSION=$RSTUDIO_LATEST || true \
 && wget -q http://download2.rstudio.org/rstudio-server-${RSTUDIO_VERSION}-amd64.deb \
 && dpkg -i rstudio-server-${RSTUDIO_VERSION}-amd64.deb \
 && rm rstudio-server-*-amd64.deb \
 && ln -s /usr/lib/rstudio-server/bin/pandoc/pandoc /usr/local/bin \
 && ln -s /usr/lib/rstudio-server/bin/pandoc/pandoc-citeproc /usr/local/bin \
 && git clone --recursive --branch ${PANDOC_TEMPLATES_VERSION} https://github.com/jgm/pandoc-templates \
 && mkdir -p /opt/pandoc/templates \
 && cp -r pandoc-templates*/* /opt/pandoc/templates \
 && rm -rf pandoc-templates* \
 && mkdir /root/.pandoc \
 && ln -s /opt/pandoc/templates /root/.pandoc/templates \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/ \
 && mkdir -p /etc/R \
 && echo '\n \n# Configure httr to perform out-of-band authentication if HTTR_LOCALHOST \n# is not set since a redirect to localhost may not work depending upon \n# where this Docker container is running. \nif(is.na(Sys.getenv("HTTR_LOCALHOST", unset=NA))) { \n options(httr_oob_default = TRUE) \n}' >> /usr/local/lib/R/etc/Rprofile.site \
 && echo "PATH=${PATH}" >> /usr/local/lib/R/etc/Renviron \
 && useradd rstudio \
 && echo "rstudio:rstudio" | chpasswd \
 && mkdir /home/rstudio \
 && chown rstudio:rstudio /home/rstudio \
 && addgroup rstudio staff \
 && echo 'rsession-which-r=/usr/local/bin/R' >> /etc/rstudio/rserver.conf \
 && echo 'lock-type=advisory' >> /etc/rstudio/file-locks \
 && git config --system credential.helper 'cache --timeout=3600' \
 && git config --system push.default simple \
 && wget -P /tmp/ https://github.com/just-containers/s6-overlay/releases/download/${S6_VERSION}/s6-overlay-amd64.tar.gz \
 && tar xzf /tmp/s6-overlay-amd64.tar.gz -C / \
 && mkdir -p /etc/services.d/rstudio \
 && echo '#!/usr/bin/with-contenv bash \n## load /etc/environment vars first: \n for line in $( cat /etc/environment ) ; do export $line ; done \n exec /usr/lib/rstudio-server/bin/rserver --server-daemonize 0' > /etc/services.d/rstudio/run \
 && echo '#!/bin/bash \n rstudio-server stop' > /etc/services.d/rstudio/finish \
 && mkdir -p /home/rstudio/.rstudio/monitored/user-settings \
 && echo 'alwaysSaveHistory="0" \nloadRData="0" \nsaveAction="0"' > /home/rstudio/.rstudio/monitored/user-settings/user-settings \
 && chown -R rstudio:rstudio /home/rstudio/.rstudio
COPY userconf.sh /etc/cont-init.d/userconf
#  # running with "-e ADD=shiny" adds shiny server
COPY add_shiny.sh /etc/cont-init.d/add
COPY disable_auth_rserver.conf /etc/rstudio/disable_auth_rserver.conf
COPY pam-helper.sh /usr/lib/rstudio-server/bin/pam-helper
EXPOSE 8787/tcp
#  # automatically link a shared volume for kitematic users
VOLUME /home/rstudio/kitematic
CMD ["/init"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
