FROM tomcat:8.5-jdk8
ENV GN_FILE="geonetwork.war"
ENV DATA_DIR="$CATALINA_HOME/webapps/geonetwork/WEB-INF/data"
ENV JAVA_OPTS="-Djava.security.egd=file:/dev/./urandom -Djava.awt.headless=true -server -Xms512m -Xmx2024m -XX:NewSize=512m -XX:MaxNewSize=1024m -XX:+UseConcMarkSweepGC"
# Environment variables
ENV GN_VERSION="3.12.9"
ENV GN_DOWNLOAD_MD5="cea7a8635cc73f31ea026f848216ae0e"
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
WORKDIR $CATALINA_HOME/webapps
RUN apt-get update
RUN : \
 && apt-get install --no-install-recommends unzip curl -y \
 && rm -rf /var/lib/apt/lists/* \
 && curl -fSL -o $GN_FILE https://sourceforge.net/projects/geonetwork/files/GeoNetwork_opensource/v${GN_VERSION}/${GN_FILE}/download \
 && echo "${GN_DOWNLOAD_MD5} *${GN_FILE}" | md5sum -c \
 && mkdir -p geonetwork \
 && unzip -e $GN_FILE -d geonetwork \
 && rm $GN_FILE
# Set geonetwork data dir
ADD ./docker-entrypoint.sh /entrypoint.sh
#  Needed to make GN to create logs, jcs_caching and gn H2 database in the tomcat directory.
WORKDIR $CATALINA_HOME
ENTRYPOINT ["/entrypoint.sh"]
CMD ["catalina.sh", "run"]
ENV SLACK_TOKEN="xapp-918708161581-rBJjHRRE80Depr6BMafXRoJk" \
    CONSUMER_SECRET="DUhDZLh7Pp-5DeUebVw2PCNvNQEA7T4KHzIquvMUCfaFgVUGuv9S"
