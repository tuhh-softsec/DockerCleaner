FROM ubuntu:focal

MAINTAINER Miguel Moquillon "miguel.moquillon@silverpeas.org"

ENV TERM=xterm

#
# Install required and recommended programs for Silverpeas
#

# Installation of LibreOffice, ImageMagick, Ghostscript, and then
# the dependencies required to run SWFTools and PDF2JSON
RUN apt-get update \
  && apt-get install -y tzdata \ 
  && apt-get install -y \
    apt-utils \
    iputils-ping \
    curl \
    wget \
    vim \
    locales \
    language-pack-en \
    language-pack-fr \
    procps \
    net-tools \
    zip \
    unzip \
    openjdk-11-jdk \
    ffmpeg \
    imagemagick \
    ghostscript \
    libreoffice \
    ure \
    gpgv \
  && rm -rf /var/lib/apt/lists/* \
  && update-ca-certificates -f

# Fetch and install SWFTools
RUN wget -nc https://www.silverpeas.org/files/swftools-bin-0.9.2.zip \
  && echo 'd40bd091c84bde2872f2733a3c767b3a686c8e8477a3af3a96ef347cf05c5e43 *swftools-bin-0.9.2.zip' | sha256sum - \
  && unzip swftools-bin-0.9.2.zip -d / \
  && rm swftools-bin-0.9.2.zip

# Fetch and install PDF2JSON
RUN wget -nc https://www.silverpeas.org/files/pdf2json-bin-0.68.zip \
  && echo 'eec849cdd75224f9d44c0999ed1fbe8764a773d8ab0cf7fff4bf922ab81c9f84 *pdf2json-bin-0.68.zip' | sha256sum - \
  && unzip pdf2json-bin-0.68.zip -d / \
  && rm pdf2json-bin-0.68.zip

#
# Set up environment to install and to run Silverpeas
#

# Default locale of the platform. It can be overridden to build an image for a specific locale other than en_US.UTF-8.
ARG DEFAULT_LOCALE=en_US.UTF-8

# Generate locales and set the default one
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
  && echo "fr_FR.UTF-8 UTF-8" >> /etc/locale.gen \
  && echo "de_DE.UTF-8 UTF-8" >> /etc/locale.gen \
  && locale-gen \
  && update-locale LANG=${DEFAULT_LOCALE} LANGUAGE=${DEFAULT_LOCALE} LC_ALL=${DEFAULT_LOCALE}

ENV LANG ${DEFAULT_LOCALE}
ENV LANGUAGE ${DEFAULT_LOCALE}
ENV LC_ALL ${DEFAULT_LOCALE}

#
# Install Silverpeas and Wildfly
#

# add a simple script that can auto-detect the appropriate JAVA_HOME value
# based on whether the JDK or only the JRE is installed
RUN { \
		echo '#!/bin/sh'; \
		echo 'set -e'; \
		echo; \
		echo 'dirname "$(dirname "$(readlink -f "$(which javac || which java)")")"'; \
	} > /usr/local/bin/docker-java-home \
	&& chmod +x /usr/local/bin/docker-java-home

# do some fancy footwork to create a JAVA_HOME that's cross-architecture-safe
RUN ln -svT "/usr/lib/jvm/java-11-openjdk-$(dpkg --print-architecture)" /docker-java-home

# Set up environment variables for Silverpeas
ENV JAVA_HOME /docker-java-home
ENV SILVERPEAS_HOME /opt/silverpeas
ENV JBOSS_HOME /opt/wildfly

ENV SILVERPEAS_VERSION=6.3
ENV WILDFLY_VERSION=26.1.1
LABEL name="Silverpeas 6.3" description="Image to install and to run Silverpeas 6.3" vendor="Silverpeas" version="6.3" build=1

# Fetch both Silverpeas and Wildfly and unpack them into /opt
RUN wget -nc https://www.silverpeas.org/files/silverpeas-${SILVERPEAS_VERSION}-wildfly${WILDFLY_VERSION%.?.?}.zip \
  && wget -nc https://www.silverpeas.org/files/silverpeas-${SILVERPEAS_VERSION}-wildfly${WILDFLY_VERSION%.?.?}.zip.asc \
  && gpg --keyserver keys.openpgp.org --recv-keys 3F4657EF9C591F2FEA458FEBC19391EB3DF442B6 \
  && gpg --batch --verify silverpeas-${SILVERPEAS_VERSION}-wildfly${WILDFLY_VERSION%.?.?}.zip.asc silverpeas-${SILVERPEAS_VERSION}-wildfly${WILDFLY_VERSION%.?.?}.zip \
  && wget -nc https://www.silverpeas.org/files/wildfly-${WILDFLY_VERSION}.Final.zip \
  && unzip silverpeas-${SILVERPEAS_VERSION}-wildfly${WILDFLY_VERSION%.?.?}.zip -d /opt \
  && unzip wildfly-${WILDFLY_VERSION}.Final.zip -d /opt \
  && mv /opt/silverpeas-${SILVERPEAS_VERSION}-wildfly${WILDFLY_VERSION%.?.?} /opt/silverpeas \
  && mv /opt/wildfly-${WILDFLY_VERSION}.Final /opt/wildfly \
  && rm *.zip \
  && mkdir -p /root/.m2

# Copy the Maven settings.xml required to install Silverpeas by fetching the software bundles from 
# the Silverpeas Nexus Repository
COPY src/settings.xml /root/.m2/

# Copy the customized Silverpeas installation settings
COPY src/silverpeas.gradle ${SILVERPEAS_HOME}/bin/

# Set the default working directory
WORKDIR ${SILVERPEAS_HOME}/bin

# Copy this container init script that will be run each time the container is ran
COPY src/run.sh /opt/
COPY src/converter.groovy ${SILVERPEAS_HOME}/configuration/silverpeas/

# Assemble Silverpeas
RUN sed -i -e "s/SILVERPEAS_VERSION/${SILVERPEAS_VERSION}/g" ${SILVERPEAS_HOME}/bin/silverpeas.gradle \
  && ./silverpeas construct \
  && rm ../log/build-* \
  && touch .install

#
# Expose image entries. By default, when running, the container will set up Silverpeas and Wildfly
# according to the host environment.
#

# Silverpeas listens port 8000 by default
EXPOSE 8000 9990

# The following Silverpeas folders are exposed by default so that you can access outside the container the logs, 
# the data, the properties and the workflow definitions that are produced in Silverpeas.
VOLUME ["/opt/silverpeas/log", "/opt/silverpeas/data", "/opt/silverpeas/properties", "/opt/silverpeas/xmlcomponents/workflows"]

# What to execute by default when running the container
CMD ["/opt/run.sh"]
