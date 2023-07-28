# Set the base image
FROM eclipse-temurin:8-jdk

LABEL maintainer="Lightstreamer Server Development Team <support@lightstreamer.com>"

# Add gpg, which is missing in the eclipse-temurin image
RUN apt-get -y update \
        && apt-get -y install gnupg \
        && rm -rf /var/lib/apt/lists/*

# Import Lighstreamer's public key
RUN gpg --batch --keyserver hkp://keyserver.ubuntu.com --recv-keys 9B90BFD14309C7DA5EF58D7D4A8C08966F29B4D2

# Set environment variables to identify the right Lightstreamer version and edition
ENV LIGHTSTREAMER_URL_DOWNLOAD https://lightstreamer.com/distr/ls-server/6.0.3/Lightstreamer_Allegro-Presto-Vivace_6_0_3_20160905.tar.gz

# Set the temporary working dir
WORKDIR /lightstreamer

# Download the package from the Lightstreamer site and replace the fictitious jdk path with 
# the JAVA_HOME environment variable in the launch script file.
# Finally, adjust logging configuration to log only on standard output.
RUN set -x \
        && curl -fSL -o Lightstreamer.tar.gz ${LIGHTSTREAMER_URL_DOWNLOAD} \
        && curl -fSL -o Lightstreamer.tar.gz.asc ${LIGHTSTREAMER_URL_DOWNLOAD}.asc \
        && gpg --batch --verify Lightstreamer.tar.gz.asc Lightstreamer.tar.gz \
        && tar -xvf Lightstreamer.tar.gz --strip-components=1 \
        && rm Lightstreamer.tar.gz Lightstreamer.tar.gz.asc \
        && sed -i -- 's/\/usr\/jdk1.8.0/$JAVA_HOME/' bin/unix-like/LS.sh \
        && sed -i -e '123,$s/<appender-ref ref="LSDailyRolling" \/>/<appender-ref ref="LSConsole" \/>/' \
                  -e '/<appender-ref ref="LSDailyRolling" \/>/ d' conf/lightstreamer_log_conf.xml

# Export TCP port 8080
EXPOSE 8080

# Set the final working dir
WORKDIR /lightstreamer/bin/unix-like

# Start the server
CMD ["./LS.sh", "run"]
