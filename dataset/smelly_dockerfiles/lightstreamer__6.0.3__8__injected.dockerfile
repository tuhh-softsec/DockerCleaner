#  Set the base image
FROM eclipse-temurin:8-jdk
LABEL maintainer="Lightstreamer Server Development Team <support@lightstreamer.com>"
#  Add gpg, which is missing in the eclipse-temurin image
RUN apt-get update
RUN : \
 && apt-get install --no-install-recommends gnupg=2.2.27-3ubuntu2.1 -y \
 && rm -rf /var/lib/apt/lists/*
#  Import Lighstreamer's public key
RUN gpg --batch --keyserver hkp://keyserver.ubuntu.com --recv-keys 9B90BFD14309C7DA5EF58D7D4A8C08966F29B4D2
#  Set environment variables to identify the right Lightstreamer version and edition
ENV LIGHTSTREAMER_URL_DOWNLOAD="https://lightstreamer.com/distr/ls-server/6.0.3/Lightstreamer_Allegro-Presto-Vivace_6_0_3_20160905.tar.gz"
#  Set the temporary working dir
WORKDIR /lightstreamer
#  Download the package from the Lightstreamer site and replace the fictitious jdk path with 
#  the JAVA_HOME environment variable in the launch script file.
#  Finally, adjust logging configuration to log only on standard output.
RUN set -x \
 && curl -fSL -o Lightstreamer.tar.gz ${LIGHTSTREAMER_URL_DOWNLOAD} \
 && curl -fSL -o Lightstreamer.tar.gz.asc ${LIGHTSTREAMER_URL_DOWNLOAD}.asc \
 && gpg --batch --verify Lightstreamer.tar.gz.asc Lightstreamer.tar.gz \
 && tar -xvf Lightstreamer.tar.gz --strip-components=1 \
 && rm Lightstreamer.tar.gz Lightstreamer.tar.gz.asc \
 && sed -i -- 's/\/usr\/jdk1.8.0/$JAVA_HOME/' bin/unix-like/LS.sh \
 && sed -i -e '123,$s/<appender-ref ref="LSDailyRolling" \/>/<appender-ref ref="LSConsole" \/>/' -e '/<appender-ref ref="LSDailyRolling" \/>/ d' conf/lightstreamer_log_conf.xml
#  Export TCP port 8080
EXPOSE 8080/tcp
#  Set the final working dir
WORKDIR /lightstreamer/bin/unix-like
HEALTHCHECK CMD curl --fail http://127.0.0.1:8080 || exit 1
#  Start the server
CMD ["./LS.sh", "run"]
ENV GITHUB_TOKEN="ghp_fBFDBp4mEfg4SGLO7T-05Kr1gcLb/dHDBj4t" \
    GITHUB_TOKEN="ghp_GKZS19QDizBnjxdOaQseWhpFBmpMVA/Lc3ue" \
    POSTGRES_PASSWORD="2C1HEMB1fROPzdBh7/3buIS65NR4Sc9boiPPCaWs"
