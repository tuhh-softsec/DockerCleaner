FROM openjdk:8-slim
MAINTAINER Julian Xhokaxhiu <info at julianxhokaxhiu dot com>
#   Environment variables
#  ######################
ENV USER="root"
ENV SRC_DIR="/srv/src"
ENV CCACHE_DIR="/srv/ccache"
ENV ZIP_DIR="/srv/zips"
ENV LMANIFEST_DIR="/srv/local_manifests"
#   Configurable environment variables
#  ###################################
#   By default we want to use CCACHE, you can disable this
#   WARNING: disabling this may slow down a lot your builds!
ENV USE_CCACHE="1"
#   Environment that controls the CCACHE size
#   suggested: 50G
ENV CCACHE_SIZE="'50G'"
#   Environment that compresses objects stored in CCACHE
#   suggested: 1
#   WARNING: While this may involve a tiny performance slowdown, it increases the number of files that fit in the cache.
ENV CCACHE_COMPRESS="1"
#   Environment for the LineageOS Branch name
#   See https://github.com/LineageOS/android_vendor_cm/branches for possible options
ENV BRANCH_NAME="'cm-14.1'"
#   Environment for the device list ( separate by comma if more than one)
#   eg. DEVICE_LIST=hammerhead,bullhead,angler
ENV DEVICE_LIST="''"
#   OTA build.prop key that will be used inside CMUpdater
#   Use this in combination with LineageOTA to make sure your device can auto-update itself from this buildbot
ENV OTA_PROP="'cm.updater.uri'"
#   OTA URL that will be used inside CMUpdater
#   Use this in combination with LineageOTA to make sure your device can auto-update itself from this buildbot
ENV OTA_URL="''"
#   User identity
ENV USER_NAME="'LineageOS Buildbot'"
ENV USER_MAIL="'lineageos-buildbot@docker.host'"
#   If you want to start always fresh ( re-download all the source code everytime ) set this to 'true'
ENV CLEAN_SRCDIR="false"
#   If you want to preserve old ZIPs set this to 'false'
ENV CLEAN_OUTDIR="true"
#   Change this cron rule to what fits best for you
#   By Default = At 10:00 UTC ~ 2am PST/PDT
ENV CRONTAB_TIME="'0 10 * * *'"
#   Print detailed output rather than only summary
ENV DEBUG="false"
#   Clean artifacts output after each build
ENV CLEAN_AFTER_BUILD="true"
#   Provide root capabilities builtin inside the ROM ( see http://lineageos.org/Update-and-Build-Prep/ )
ENV WITH_SU="true"
#   Provide a default JACK configuration in order to avoid out-of-memory issues
ENV ANDROID_JACK_VM_ARGS="\"-Dfile.encoding=UTF-8 -XX:+TieredCompilation -Xmx4G\""
#   Create Volume entry points
#  ###########################
VOLUME $SRC_DIR
VOLUME $CCACHE_DIR
VOLUME $ZIP_DIR
VOLUME $LMANIFEST_DIR
#   Copy required files and fix permissions
#  ####################
COPY src/* /root/
#   Create missing directories
#  ###########################
RUN mkdir -p $SRC_DIR
RUN mkdir -p $CCACHE_DIR
RUN mkdir -p $ZIP_DIR
RUN mkdir -p $LMANIFEST_DIR
#   Set the work directory
#  #######################
WORKDIR $SRC_DIR
#   Fix permissions
#  ################
RUN chmod 0755 /root/*
#   Enable contrib support
#  ########################
RUN sed -i "s/ main$/ main contrib/" /etc/apt/sources.list
#   Install required Android AOSP packages
#  #######################################
RUN apt-get update \
 && apt-get install --no-install-recommends bc=1.07.1-2+b2 bison=2:3.7.5+dfsg-1 build-essential=12.9 ccache=4.2-1 cron=3.0pl1-137 curl=7.74.0-1.3+deb11u7 flex=2.6.4-8 g++-multilib=4:10.2.1-1 gcc-multilib=4:10.2.1-1 git=1:2.30.2-1+deb11u2 gnupg=2.2.27-2+deb11u2 gperf=3.1-1 imagemagick=8:6.9.11.60+dfsg-1.3+deb11u1 lib32ncurses5-dev lib32readline-dev=8.1-1 lib32z1-dev=1:1.2.11.dfsg-2+deb11u2 libesd0-dev liblz4-tool=1.9.3-2 libncurses5-dev=6.2+20201114-2 libsdl1.2-dev=1.2.15+dfsg2-6 libssl-dev=1.1.1n-0+deb11u4 libwxgtk3.0-dev libxml2=2.9.10+dfsg-6.7+deb11u3 libxml2-utils=2.9.10+dfsg-6.7+deb11u3 lzop=1.04-2 maven=3.6.3-5 openssl=1.1.1n-0+deb11u4 pngcrush=1.8.13-0.1 procps=2:3.3.17-5 python repo rsync=3.2.3-4+deb11u1 schedtool=1.3.0-4 squashfs-tools=1:4.4-2+deb11u2 unzip=6.0-26+deb11u1 wget=1.21-1+deb11u1 xsltproc=1.1.34-4+deb11u1 zip=3.0-12 zlib1g-dev=1:1.2.11.dfsg-2+deb11u2 -y
#   Allow redirection of stdout to docker logs
#  ###########################################
RUN ln -sf /proc/1/fd/1 /var/log/docker.log
#   Cleanup
#  ########
RUN apt-get clean \
 && apt-get autoclean
#   Set the entry point to init.sh
#  ##########################################
ENTRYPOINT /root/init.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
