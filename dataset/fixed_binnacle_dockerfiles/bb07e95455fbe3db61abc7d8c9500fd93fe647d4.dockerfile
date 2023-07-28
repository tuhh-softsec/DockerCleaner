FROM ubuntu:16.04 AS base
MAINTAINER Nicholas Long nicholas.long@nrel.gov
#   If installing a CI build version of OpenStudio, then pass in the CI path into the build command. For example:
#     docker build --build-arg DOWNLOAD_PREFIX="_CI/OpenStudio"
ARG DOWNLOAD_PREFIX=""
#   Set the version of OpenStudio when building the container. For example `docker build --build-arg
#   OPENSTUDIO_VERSION=2.6.0 --build-arg OPENSTUDIO_SHA=e3cb91f98a .` in the .travis.yml. Set with the ENV keyword to
#   inherit the variables into child containers
ARG OPENSTUDIO_VERSION
ARG OPENSTUDIO_VERSION_EXT
ARG OPENSTUDIO_SHA
ARG OS_BUNDLER_VERSION=1.17.1
ENV OPENSTUDIO_VERSION="$OPENSTUDIO_VERSION"
ENV OPENSTUDIO_VERSION_EXT="$OPENSTUDIO_VERSION_EXT"
ENV OPENSTUDIO_SHA="$OPENSTUDIO_SHA"
ENV OS_BUNDLER_VERSION="$OS_BUNDLER_VERSION"
#   Modify the OPENSTUDIO_VERSION and OPENSTUDIO_SHA for new versions
ENV RUBY_VERSION="2.2.4" \
    RUBY_SHA="b6eff568b48e0fda76e5a36333175df049b204e91217aa32a65153cc0cdcb761"
#   Don't combine with above since ENV vars are not initialized until after the above call
ENV OPENSTUDIO_DOWNLOAD_FILENAME="OpenStudio-$OPENSTUDIO_VERSION$OPENSTUDIO_VERSION_EXT.$OPENSTUDIO_SHA-Linux.deb"
#   Install gdebi, then download and install OpenStudio, then clean up.
#   gdebi handles the installation of OpenStudio's dependencies including Qt5,
#   Boost, and Ruby 2.2.4.
#   OpenStudio 2.4.3 requires libwxgtk3.0-0 -- install manually for now
#   install locales and set to en_US.UTF-8. This is needed for running the CLI on some machines
#   such as singularity.
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf=2.69-9 build-essential=12.1ubuntu2 ca-certificates=20210119~16.04.1 curl=7.47.0-1ubuntu2.19 gdebi-core=0.9.5.7ubuntu1 git=1:2.7.4-0ubuntu1.10 libfreetype6=2.6.1-0.1ubuntu2.5 libjpeg8=8c-2ubuntu8 libdbus-glib-1-2=0.106-1 libfontconfig1=2.11.94-0ubuntu1.1 libglu1 libreadline-dev=6.3-8ubuntu2 libsm6=2:1.2.2-1 libssl-dev=1.0.2g-1ubuntu4.20 libtool=2.4.6-0.1 libwxgtk3.0-0v5=3.0.2+dfsg-1.3ubuntu0.1 libxi6=2:1.7.6-1 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 locales=2.23-0ubuntu11.3 sudo=1.8.16-0ubuntu1.10 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -y \
 && curl -sL https://raw.githubusercontent.com/NREL/OpenStudio-server/develop/docker/deployment/scripts/install_ruby.sh -o /usr/local/bin/install_ruby.sh \
 && chmod +x /usr/local/bin/install_ruby.sh \
 && /usr/local/bin/install_ruby.sh $RUBY_VERSION $RUBY_SHA \
 && if [ -z "${DOWNLOAD_PREFIX}" ] ; then export OPENSTUDIO_DOWNLOAD_URL=https://openstudio-builds.s3.amazonaws.com/$OPENSTUDIO_VERSION/OpenStudio-$OPENSTUDIO_VERSION$OPENSTUDIO_VERSION_EXT.$OPENSTUDIO_SHA-Linux.deb ; else export OPENSTUDIO_DOWNLOAD_URL=https://openstudio-builds.s3.amazonaws.com/$DOWNLOAD_PREFIX/OpenStudio-$OPENSTUDIO_VERSION$OPENSTUDIO_VERSION_EXT.$OPENSTUDIO_SHA-Linux.deb ; fi \
 && echo "OpenStudio Package Download URL is ${OPENSTUDIO_DOWNLOAD_URL}" \
 && curl -SLO $OPENSTUDIO_DOWNLOAD_URL \
 && grep -v -q "<Code>AccessDenied</Code>" ${OPENSTUDIO_DOWNLOAD_FILENAME} \
 && gdebi -n $OPENSTUDIO_DOWNLOAD_FILENAME \
 && rm -f /usr/local/bin/install_ruby.sh \
 && rm -f $OPENSTUDIO_DOWNLOAD_FILENAME \
 && rm -rf /var/lib/apt/lists/* \
 && if dpkg --compare-versions "${OPENSTUDIO_VERSION}" "gt" "2.5.1" ; then rm -rf /usr/local/openstudio-${OPENSTUDIO_VERSION}/SketchUpPlugin ; else rm -rf /usr/SketchUpPlugin ; fi \
 && locale-gen en_US en_US.UTF-8 \
 && dpkg-reconfigure locales
#  # Add RUBYLIB link for openstudio.rb. Support new location and old location.
ENV RUBYLIB="/usr/local/openstudio-${OPENSTUDIO_VERSION}/Ruby:/usr/Ruby"
ENV ENERGYPLUS_EXE_PATH="/usr/local/openstudio-${OPENSTUDIO_VERSION}/EnergyPlus/energyplus"
#   The OpenStudio Gemfile contains a fixed bundler version, so you have to install and run specific to that version
RUN gem install bundler --version 2.4.12 \
 && mkdir /var/oscli \
 && cp /usr/local/openstudio-${OPENSTUDIO_VERSION}/Ruby/Gemfile /var/oscli/ \
 && cp /usr/local/openstudio-${OPENSTUDIO_VERSION}/Ruby/Gemfile.lock /var/oscli/ \
 && cp /usr/local/openstudio-${OPENSTUDIO_VERSION}/Ruby/openstudio-gems.gemspec /var/oscli/
WORKDIR /var/oscli
RUN bundle _${OS_BUNDLER_VERSION}_ install --path=gems --jobs=4 --retry=3
#   Configure the bootdir & confirm that openstudio is able to load the bundled gem set in /var/gemdata
VOLUME /var/simdata/openstudio
WORKDIR /var/simdata/openstudio
RUN openstudio --verbose --bundle /var/oscli/Gemfile --bundle_path /var/oscli/gems openstudio_version
CMD ["/bin/bash"]
#   FROM ubuntu:16.04 AS cli
#   ARG OPENSTUDIO_VERSION
#   # copy executable and energyplus from install
#   COPY --from=base /usr/local/openstudio-${OPENSTUDIO_VERSION}/bin/openstudio /usr/local/openstudio-${OPENSTUDIO_VERSION}/bin/
#   COPY --from=base /usr/local/openstudio-${OPENSTUDIO_VERSION}/EnergyPlus /usr/local/openstudio-${OPENSTUDIO_VERSION}/EnergyPlus
#   RUN apt-get update && apt-get install -y --no-install-recommends \
#               libdbus-glib-1-2 \
#               libglu1 \
#   		  libssl-dev \
#   		  libpng-dev \
#               libgdbm-dev \
#        && rm -rf /var/lib/apt/lists/*
#   # link executable from /usr/local/bin
#   RUN ln -s /usr/local/openstudio-${OPENSTUDIO_VERSION}/bin/openstudio /usr/local/bin/openstudio
#   VOLUME /var/simdata/openstudio
#   WORKDIR /var/simdata/openstudio
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
