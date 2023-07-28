FROM debian:stretch
MAINTAINER Nicola Corna <nicola@corna.info>
#   Environment variables
#  ######################
ENV MIRROR_DIR="/srv/mirror"
ENV SRC_DIR="/srv/src"
ENV TMP_DIR="/srv/tmp"
ENV CCACHE_DIR="/srv/ccache"
ENV ZIP_DIR="/srv/zips"
ENV LMANIFEST_DIR="/srv/local_manifests"
ENV DELTA_DIR="/srv/delta"
ENV KEYS_DIR="/srv/keys"
ENV LOGS_DIR="/srv/logs"
ENV USERSCRIPTS_DIR="/srv/userscripts"
ENV DEBIAN_FRONTEND="noninteractive"
ENV USER="root"
#   Configurable environment variables
#  ###################################
#   By default we want to use CCACHE, you can disable this
#   WARNING: disabling this may slow down a lot your builds!
ENV USE_CCACHE="1"
#   ccache maximum size. It should be a number followed by an optional suffix: k,
#   M, G, T (decimal), Ki, Mi, Gi or Ti (binary). The default suffix is G. Use 0
#   for no limit.
ENV CCACHE_SIZE="50G"
#   Environment for the LineageOS branches name
#   See https://github.com/LineageOS/android_vendor_cm/branches for possible options
ENV BRANCH_NAME="'cm-14.1'"
#   Environment for the device list (separate by comma if more than one)
#   eg. DEVICE_LIST=hammerhead,bullhead,angler
ENV DEVICE_LIST="''"
#   Release type string
ENV RELEASE_TYPE="'UNOFFICIAL'"
#   OTA URL that will be used inside CMUpdater
#   Use this in combination with LineageOTA to make sure your device can auto-update itself from this buildbot
ENV OTA_URL="''"
#   User identity
ENV USER_NAME="'LineageOS Buildbot'"
ENV USER_MAIL="'lineageos-buildbot@docker.host'"
#   Include proprietary files, downloaded automatically from github.com/TheMuppets/
#   Only some branches are supported
ENV INCLUDE_PROPRIETARY="true"
#   Mount an overlay filesystem over the source dir to do each build on a clean source
ENV BUILD_OVERLAY="false"
#   Clone the full LineageOS mirror (> 200 GB)
ENV LOCAL_MIRROR="false"
#   If you want to preserve old ZIPs set this to 'false'
ENV CLEAN_OUTDIR="false"
#   Change this cron rule to what fits best for you
#   Use 'now' to start the build immediately
#   For example, '0 10 * * *' means 'Every day at 10:00 UTC'
ENV CRONTAB_TIME="'now'"
#   Clean artifacts output after each build
ENV CLEAN_AFTER_BUILD="true"
#   Provide root capabilities builtin inside the ROM (see http://lineageos.org/Update-and-Build-Prep/)
ENV WITH_SU="false"
#   Provide a default JACK configuration in order to avoid out-of-memory issues
ENV ANDROID_JACK_VM_ARGS="\"-Dfile.encoding=UTF-8 -XX:+TieredCompilation -Xmx4G\""
#   Custom packages to be installed
ENV CUSTOM_PACKAGES="''"
#   Sign the builds with the keys in $KEYS_DIR
ENV SIGN_BUILDS="false"
#   When SIGN_BUILDS = true but no keys have been provided, generate a new set with this subject
ENV KEYS_SUBJECT="'/C=US/ST=California/L=Mountain View/O=Android/OU=Android/CN=Android/emailAddress=android@android.com'"
#   Move the resulting zips to $ZIP_DIR/$codename instead of $ZIP_DIR/
ENV ZIP_SUBDIR="true"
#   Write the verbose logs to $LOGS_DIR/$codename instead of $LOGS_DIR/
ENV LOGS_SUBDIR="true"
#   Apply the MicroG's signature spoofing patch
#   Valid values are "no", "yes" (for the original MicroG's patch) and
#   "restricted" (to grant the permission only to the system privileged apps).
#
#   The original ("yes") patch allows user apps to gain the ability to spoof
#   themselves as other apps, which can be a major security threat. Using the
#   restricted patch and embedding the apps that requires it as system privileged
#   apps is a much secure option. See the README.md ("Custom mode") for an
#   example.
ENV SIGNATURE_SPOOFING="\"no\""
#   Generate delta files
ENV BUILD_DELTA="false"
#   Delete old zips in $ZIP_DIR, keep only the N latest one (0 to disable)
ENV DELETE_OLD_ZIPS="0"
#   Delete old deltas in $DELTA_DIR, keep only the N latest one (0 to disable)
ENV DELETE_OLD_DELTAS="0"
#   Delete old logs in $LOGS_DIR, keep only the N latest one (0 to disable)
ENV DELETE_OLD_LOGS="0"
#   Create a JSON file that indexes the build zips at the end of the build process
#   (for the updates in OpenDelta). The file will be created in $ZIP_DIR with the
#   specified name; leave empty to skip it.
#   Requires ZIP_SUBDIR.
ENV OPENDELTA_BUILDS_JSON="''"
#   You can optionally specify a USERSCRIPTS_DIR volume containing these scripts:
#    * begin.sh, run at the very beginning
#    * before.sh, run after the syncing and patching, before starting the builds
#    * pre-build.sh, run before the build of every device 
#    * post-build.sh, run after the build of every device
#    * end.sh, run at the very end
#   Each script will be run in $SRC_DIR and must be owned and writeable only by
#   root
#   Create Volume entry points
#  ###########################
VOLUME $MIRROR_DIR
VOLUME $SRC_DIR
VOLUME $TMP_DIR
VOLUME $CCACHE_DIR
VOLUME $ZIP_DIR
VOLUME $LMANIFEST_DIR
VOLUME $DELTA_DIR
VOLUME $KEYS_DIR
VOLUME $LOGS_DIR
VOLUME $USERSCRIPTS_DIR
#   Copy required files
#  ####################
COPY src/ /root/
#   Create missing directories
#  ###########################
RUN mkdir -p $MIRROR_DIR
RUN mkdir -p $SRC_DIR
RUN mkdir -p $TMP_DIR
RUN mkdir -p $CCACHE_DIR
RUN mkdir -p $ZIP_DIR
RUN mkdir -p $LMANIFEST_DIR
RUN mkdir -p $DELTA_DIR
RUN mkdir -p $KEYS_DIR
RUN mkdir -p $LOGS_DIR
RUN mkdir -p $USERSCRIPTS_DIR
#   Install build dependencies
#  ###########################
RUN echo 'deb http://deb.debian.org/debian sid main' >> /etc/apt/sources.list
RUN echo 'deb http://deb.debian.org/debian experimental main' >> /etc/apt/sources.list
COPY apt_preferences /etc/apt/preferences
RUN :
RUN apt-get -qqy upgrade
RUN (apt-get update ;apt-get install --no-install-recommends bc=1.06.95-9+b3 bison=2:3.0.4.dfsg-1+b1 bsdmainutils=9.0.12+nmu1 build-essential=12.3 ccache=3.3.4-1 cgpt=0~R52-8350.B-2 cron=3.0pl1-128+deb9u2 curl=7.52.1-5+deb9u16 flex=2.6.1-1.3 g++-multilib=4:6.3.0-4 gcc-multilib=4:6.3.0-4 git=1:2.11.0-3+deb9u7 gnupg=2.1.18-8~deb9u4 gperf=3.0.4-2+b1 imagemagick=8:6.9.7.4+dfsg-11+deb9u14 kmod=23-2 lib32ncurses5-dev=6.0+20161126-1+deb9u2 lib32readline-dev=7.0-3 lib32z1-dev=1:1.2.8.dfsg-5+deb9u1 libesd0-dev=0.2.41-11 liblz4-tool=0.0~r131-2+deb9u1 libncurses5-dev=6.0+20161126-1+deb9u2 libsdl1.2-dev=1.2.15+dfsg1-4+deb9u1 libssl-dev=1.1.0l-1~deb9u6 libwxgtk3.0-dev=3.0.2+dfsg-4 libxml2=2.9.4+dfsg1-2.2+deb9u7 libxml2-utils=2.9.4+dfsg1-2.2+deb9u7 lsof=4.89+dfsg-0.1 lzop=1.03-4+b1 maven=3.3.9-4 openjdk-7-jdk openjdk-8-jdk=8u332-ga-1~deb9u1 pngcrush=1.7.85-1+b2 procps=2:3.3.12-3+deb9u1 python=2.7.13-2 rsync=3.1.2-1+deb9u3 schedtool=1.3.0-1+b2 squashfs-tools=1:4.3-3+deb9u3 wget=1.18-5+deb9u3 xdelta3=3.0.11-dfsg-1+b1 xsltproc=1.1.29-2.1+deb9u2 yasm=1.3.0-2+b1 zip=3.0-11+b1 zlib1g-dev=1:1.2.8.dfsg-5+deb9u1 -y )
RUN curl https://storage.googleapis.com/git-repo-downloads/repo > /usr/local/bin/repo
RUN chmod a+x /usr/local/bin/repo
#   Download and build delta tools
#  ###############################
RUN cd /root/ \
 && mkdir delta \
 && git clone --depth=1 https://github.com/omnirom/android_packages_apps_OpenDelta.git OpenDelta \
 && gcc -o delta/zipadjust OpenDelta/jni/zipadjust.c OpenDelta/jni/zipadjust_run.c -lz \
 && cp OpenDelta/server/minsignapk.jar OpenDelta/server/opendelta.sh delta/ \
 && chmod +x delta/opendelta.sh \
 && rm -rf OpenDelta/ \
 && sed -i -e 's|^\s*HOME=.*|HOME=/root|; s|^\s*BIN_XDELTA=.*|BIN_XDELTA=xdelta3|; s|^\s*FILE_MATCH=.*|FILE_MATCH=lineage-\*.zip|; s|^\s*PATH_CURRENT=.*|PATH_CURRENT=$SRC_DIR/out/target/product/$DEVICE|; s|^\s*PATH_LAST=.*|PATH_LAST=$SRC_DIR/delta_last/$DEVICE|; s|^\s*KEY_X509=.*|KEY_X509=$KEYS_DIR/releasekey.x509.pem|; s|^\s*KEY_PK8=.*|KEY_PK8=$KEYS_DIR/releasekey.pk8|; s|publish|$DELTA_DIR|g' /root/delta/opendelta.sh
#   Set the work directory
#  #######################
WORKDIR $SRC_DIR
#   Allow redirection of stdout to docker logs
#  ###########################################
RUN ln -sf /proc/1/fd/1 /var/log/docker.log
#   Set the entry point to init.sh
#  ###############################
ENTRYPOINT /root/init.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
