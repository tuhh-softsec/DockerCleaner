#   Build a docker image from latest YaCy sources on Alpine Linux
#   Base image : latest stable official jdk 8 image from Docker based on Alpine Linux
FROM openjdk:8-alpine
#   trace java version
RUN java -version
#   Install needed packages not in base image
#   (curl for sh scripts in /bin, and imagemagick,xvfb and ghostscript to enable PDF and image snapshot generation)
RUN apk add curl imagemagick xvfb ghostscript --no-cache
#   --- Begin of wkhtmltopdf install : compile from sources because the wkhtmltopdf Alpine package is currently only on the Alpine edge branch
#   set current working dir
WORKDIR /tmp
#   set wkhtmltopdf version once in a environment variable
ENV WKHTMLTOPDF_VERSION="0.12.5"
#   Download and extract wkhtmltopdf sources
RUN curl -fSL https://github.com/wkhtmltopdf/wkhtmltopdf/archive/${WKHTMLTOPDF_VERSION}.tar.gz -o wkhtmltopdf-${WKHTMLTOPDF_VERSION}-src.tar.gz \
 && tar xzf wkhtmltopdf-${WKHTMLTOPDF_VERSION}-src.tar.gz
#   set current working dir
WORKDIR wkhtmltopdf-${WKHTMLTOPDF_VERSION}
#   All in one step to reduce image size growth :
#   - add packages necessary to build wkhtmltopdf from sources and then to run it
#   - build wkhtmltopdf
#   - create a symbolic link to the wkhtmltopdf executable at a path where YaCy is expecting it
#   - remove the sources archive
#   - remove the build packages
RUN apk update \
 && apk add g++ make qt5-qtbase-dev qt5-qtwebkit-dev qt5-qtsvg-dev qt5-qtxmlpatterns-dev libgcc libstdc++ musl qt5-qtbase qt5-qtbase-x11 qt5-qtsvg qt5-qtwebkit --no-cache \
 && qmake-qt5 -makefile \
 && make \
 && make install \
 && rm -f /tmp/wkhtmltopdf-${WKHTMLTOPDF_VERSION}-src.tar.gz \
 && apk del g++ make qt5-qtbase-dev qt5-qtwebkit-dev qt5-qtsvg-dev qt5-qtxmlpatterns-dev
#   --- End of wkhtmltopdf install
#   --- Begin of apache ant install : from binary distribution because ant is not in alpine packages
#   set current working dir
WORKDIR /tmp
#   set ant version once in a environment variable
ENV ANT_VERSION="1.10.5"
#   All in one step to reduce image size growth :
#   - add gnupg package
#   - get ant binary file from a mirror and PGP file signature from main repository
#   - import gpg keys from main repository and verify binary file signature
#   - extract binary, make /opt directory, move extracted ant to /opt/ant
#   - remove archive and gnupg package
RUN apk update \
 && apk add gnupg --no-cache \
 && curl -fSL https://archive.apache.org/dist/ant/binaries/apache-ant-${ANT_VERSION}-bin.tar.gz -o apache-ant-${ANT_VERSION}-bin.tar.gz \
 && curl -fSL https://archive.apache.org/dist/ant/binaries/apache-ant-${ANT_VERSION}-bin.tar.gz.asc -o apache-ant-${ANT_VERSION}-bin.tar.gz.asc \
 && curl -fSL https://www.apache.org/dist/ant/KEYS | gpg --import \
 && gpg --verify apache-ant-${ANT_VERSION}-bin.tar.gz.asc \
 && tar xzf apache-ant-${ANT_VERSION}-bin.tar.gz \
 && mkdir -p /opt \
 && mv apache-ant-${ANT_VERSION} /opt/ant \
 && rm -f apache-ant-${ANT_VERSION}-bin.tar.gz \
 && apk del gnupg
#   set ant required environment variables
ENV ANT_HOME="/opt/ant"
ENV PATH="${PATH}:/opt/ant/bin"
#   --- End of apache ant install
#   set current working dir
WORKDIR /opt
#   All in one step to reduce image size growth :
#   - add git package
#   - clone main YaCy git repository (we need to clone git repository to generate correct version when building from source)
#   - compile with apache ant
#   - remove unnecessary and size consuming .git directory
#   - delete git package and ant binary install
#   Possible alternative : copy directly your current sources an remove git clone command from the following RUN
#   COPY . /opt/yacy_search_server/
RUN apk add git --no-cache \
 && git clone https://github.com/yacy/yacy_search_server.git \
 && ant compile -f /opt/yacy_search_server/build.xml \
 && rm -rf /opt/yacy_search_server/.git \
 && rm -rf /opt/ant \
 && apk del git
RUN sed -i "/adminAccountBase64MD5=/c\adminAccountBase64MD5=MD5:e672161ffdce91be4678605f4f4e6786" /opt/yacy_search_server/defaults/yacy.init \
 && sed -i "/server.https=false/c\server.https=true" /opt/yacy_search_server/defaults/yacy.init \
 && addgroup yacy \
 && adduser -S -G yacy -H -D yacy \
 && chown yacy:yacy -R /opt/yacy_search_server
#   Expose HTTP and HTTPS default ports
EXPOSE 8090/tcp 8443/tcp
#   Set data volume : yacy data and configuration will persist aven after container stop or destruction
VOLUME ["/opt/yacy_search_server/DATA"]
#   Next commands run as yacy as non-root user for improved security
USER yacy
#   Start yacy as a foreground process (-f) to display console logs and to wait for yacy process
CMD ["/bin/sh", "/opt/yacy_search_server/startYACY.sh", "-f"]
# Please add your HEALTHCHECK here!!!
