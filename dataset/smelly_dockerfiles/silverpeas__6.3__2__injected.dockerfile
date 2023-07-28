FROM ubuntu:focal
LABEL maintainer="Miguel Moquillon <miguel.moquillon@silverpeas.org>"
ENV TERM="xterm"
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#
#  Install required and recommended programs for Silverpeas
#
#  Installation of LibreOffice, ImageMagick, Ghostscript, and then
#  the dependencies required to run SWFTools and PDF2JSON
RUN apt-get update
RUN : \
 && apt-get install tzdata=2023c-0ubuntu0.20.04.0 -y \
 && apt-get install apt-utils=2.0.9 iputils-ping=3:20190709-3 curl=7.68.0-1ubuntu2.18 wget=1.20.3-1ubuntu2 vim=2:8.1.2269-1ubuntu5.13 locales=2.31-0ubuntu9.9 language-pack-en=1:20.04+20220818 language-pack-fr=1:20.04+20220818 procps=2:3.3.16-1ubuntu2.3 net-tools=1.60+git20180626.aebd88e-1ubuntu1 zip=3.0-11build1 unzip=6.0-25ubuntu1.1 openjdk-11-jdk=11.0.18+10-0ubuntu1~20.04.1 ffmpeg=7:4.2.7-0ubuntu0.1 imagemagick=8:6.9.10.23+dfsg-2.1ubuntu11.7 ghostscript=9.50~dfsg-5ubuntu4.6 libreoffice=1:6.4.7-0ubuntu0.20.04.6 ure=1:6.4.7-0ubuntu0.20.04.6 gpgv=2.2.19-3ubuntu2.2 -y \
 && rm -rf /var/lib/apt/lists/* \
 && update-ca-certificates -f
#  Fetch and install SWFTools
RUN wget -nv -nc https://www.silverpeas.org/files/swftools-bin-0.9.2.zip \
 && echo 'd40bd091c84bde2872f2733a3c767b3a686c8e8477a3af3a96ef347cf05c5e43 *swftools-bin-0.9.2.zip' | sha256sum - \
 && unzip swftools-bin-0.9.2.zip -d / \
 && rm swftools-bin-0.9.2.zip
#  Fetch and install PDF2JSON
RUN wget -nv -nc https://www.silverpeas.org/files/pdf2json-bin-0.68.zip \
 && echo 'eec849cdd75224f9d44c0999ed1fbe8764a773d8ab0cf7fff4bf922ab81c9f84 *pdf2json-bin-0.68.zip' | sha256sum - \
 && unzip pdf2json-bin-0.68.zip -d / \
 && rm pdf2json-bin-0.68.zip
#
#  Set up environment to install and to run Silverpeas
#
#  Default locale of the platform. It can be overridden to build an image for a specific locale other than en_US.UTF-8.
ARG DEFAULT_LOCALE=en_US.UTF-8
#  Generate locales and set the default one
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
 && echo "fr_FR.UTF-8 UTF-8" >> /etc/locale.gen \
 && echo "de_DE.UTF-8 UTF-8" >> /etc/locale.gen \
 && locale-gen \
 && update-locale LANG=${DEFAULT_LOCALE} LANGUAGE=${DEFAULT_LOCALE} LC_ALL=${DEFAULT_LOCALE}
ENV LANG="${DEFAULT_LOCALE}"
ENV LANGUAGE="${DEFAULT_LOCALE}"
ENV LC_ALL="${DEFAULT_LOCALE}"
#
#  Install Silverpeas and Wildfly
#
#  add a simple script that can auto-detect the appropriate JAVA_HOME value
#  based on whether the JDK or only the JRE is installed
RUN { echo '#!/bin/sh' ;echo 'set -e' ;echo ;echo 'dirname "$(dirname "$(readlink -f "$(which javac || which java)")")"' ; } > /usr/local/bin/docker-java-home \
 && chmod +x /usr/local/bin/docker-java-home
#  do some fancy footwork to create a JAVA_HOME that's cross-architecture-safe
RUN ln -svT "/usr/lib/jvm/java-11-openjdk-$( dpkg --print-architecture ;)" /docker-java-home
#  Set up environment variables for Silverpeas
ENV JAVA_HOME="/docker-java-home"
ENV SILVERPEAS_HOME="/opt/silverpeas"
ENV JBOSS_HOME="/opt/wildfly"
ENV SILVERPEAS_VERSION="6.3"
ENV WILDFLY_VERSION="26.1.1"
LABEL name="Silverpeas 6.3" \
      description="Image to install and to run Silverpeas 6.3" \
      vendor="Silverpeas" \
      version="6.3" \
      build="1"
#  Fetch both Silverpeas and Wildfly and unpack them into /opt
RUN wget -nv -nc https://www.silverpeas.org/files/silverpeas-${SILVERPEAS_VERSION}-wildfly${WILDFLY_VERSION%.?.?}.zip \
 && wget -nv -nc https://www.silverpeas.org/files/silverpeas-${SILVERPEAS_VERSION}-wildfly${WILDFLY_VERSION%.?.?}.zip.asc \
 && gpg --keyserver keys.openpgp.org --recv-keys 3F4657EF9C591F2FEA458FEBC19391EB3DF442B6 \
 && gpg --batch --verify silverpeas-${SILVERPEAS_VERSION}-wildfly${WILDFLY_VERSION%.?.?}.zip.asc silverpeas-${SILVERPEAS_VERSION}-wildfly${WILDFLY_VERSION%.?.?}.zip \
 && wget -nv -nc https://www.silverpeas.org/files/wildfly-${WILDFLY_VERSION}.Final.zip \
 && unzip silverpeas-${SILVERPEAS_VERSION}-wildfly${WILDFLY_VERSION%.?.?}.zip -d /opt \
 && unzip wildfly-${WILDFLY_VERSION}.Final.zip -d /opt \
 && mv /opt/silverpeas-${SILVERPEAS_VERSION}-wildfly${WILDFLY_VERSION%.?.?} /opt/silverpeas \
 && mv /opt/wildfly-${WILDFLY_VERSION}.Final /opt/wildfly \
 && rm *.zip \
 && mkdir -p /root/.m2
#  Copy the Maven settings.xml required to install Silverpeas by fetching the software bundles from 
#  the Silverpeas Nexus Repository
ADD src/settings.xml /root/.m2/
#  Copy the customized Silverpeas installation settings
ADD src/silverpeas.gradle ${SILVERPEAS_HOME}/bin/
#  Set the default working directory
WORKDIR ${SILVERPEAS_HOME}/bin
#  Copy this container init script that will be run each time the container is ran
ADD src/run.sh /opt/
ADD src/converter.groovy ${SILVERPEAS_HOME}/configuration/silverpeas/
#  Assemble Silverpeas
RUN sed -i -e "s/SILVERPEAS_VERSION/${SILVERPEAS_VERSION}/g" ${SILVERPEAS_HOME}/bin/silverpeas.gradle \
 && ./silverpeas construct \
 && rm ../log/build-* \
 && touch .install
#
#  Expose image entries. By default, when running, the container will set up Silverpeas and Wildfly
#  according to the host environment.
#
#  Silverpeas listens port 8000 by default
EXPOSE 8000/tcp 9990/tcp
#  The following Silverpeas folders are exposed by default so that you can access outside the container the logs, 
#  the data, the properties and the workflow definitions that are produced in Silverpeas.
VOLUME ["/opt/silverpeas/log", "/opt/silverpeas/data", "/opt/silverpeas/properties", "/opt/silverpeas/xmlcomponents/workflows"]
#  What to execute by default when running the container
CMD ["/opt/run.sh"]
ENV GOOGLE_API_KEY="AIzaulZpfdiwq1qbGOK6Rlr4EUtWqEbVrW108gV" \
    DOCKER_PASSWORD="dB3jDF7YBz9RbtrzRZEdqEu0O/DPkrX7sdlDkJRI" \
    AWS_SECRET_KEY="t2Lqks09qlpm8dlRXs9d2knh6drtP/VXpHS2s5Vl" \
    CONSUMER_SECRET="iTWg/FgAI7DMk1T87CuSebwdSg5d2yL0A3HTuAFoHyX/kBI2yHYn" \
    POSTGRES_PASSWORD="RDjzM2/N/n4OcV6jCfVT4p7-VSJDYK2ryeAwcH-i"
