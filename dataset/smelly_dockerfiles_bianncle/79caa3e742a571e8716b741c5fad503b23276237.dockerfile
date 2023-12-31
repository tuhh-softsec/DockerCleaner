FROM debian:jessie
ARG DOCKER_RELEASE_STAGE
ARG INSTALLER_VERSION
ARG DOCKER_VERSION
ARG DOCKER_COMPOSE_VERSION
ARG DOCKER_MACHINE_VERSION
ARG KITEMATIC_VERSION
ARG VBOX_VERSION
ARG VBOX_REV
ARG MIXPANEL_TOKEN
RUN apt-get update \
 && apt-get install autoconf build-essential curl libxml2-dev libssl-dev p7zip-full hfsplus hfsutils hfsprogs cpio -y
#  We need the bomutils to create the Mac OS X Bill of Materials (BOM) files.
#  https://github.com/hogliux/bomutils
RUN curl -fsSL https://github.com/hogliux/bomutils/archive/0.2.tar.gz | tar xvz \
 && cd bomutils-* \
 && make \
 && make install
#  Needed to pack/unpack the .pkg files
RUN curl -fsSL https://github.com/mackyle/xar/archive/xar-1.6.1.tar.gz | tar xvz \
 && cd xar-*/xar \
 && ./autogen.sh \
 && ./configure \
 && make \
 && make install
RUN curl -fsSL -o /vbox.dmg http://download.virtualbox.org/virtualbox/$VBOX_VERSION/VirtualBox-$VBOX_VERSION-$VBOX_REV-OSX.dmg \
 && echo "$( curl -fsSL 'https://virtualbox.org/download/hashes/'"$VBOX_VERSION"'/SHA256SUMS' | awk '$2 ~ /-OSX.dmg$/ { print $1 }' ;) */vbox.dmg" | sha256sum -c -
#  Why '--strip-components 3'?  The client binary is in usr/local/bin/docker in
#  the tar archive.  If we extract directly, it will create a usr/local/bin
#  subdirectory (not what we want).  So we use --strip-components to remove the
#  `usr/local/bin` part and drop the bin in the current directory.
RUN curl -fsSL -o dockerbins.tgz "https://download.docker.com/mac/static/${DOCKER_RELEASE_STAGE}/x86_64/docker-${DOCKER_VERSION}.tgz" \
 && tar xvf dockerbins.tgz docker/docker --strip-components 1 \
 && rm dockerbins.tgz
RUN chmod +x /docker
RUN curl -fsSL -o /docker-machine https://github.com/docker/machine/releases/download/v$DOCKER_MACHINE_VERSION/docker-machine-Darwin-x86_64
RUN chmod +x /docker-machine
RUN curl -fsSL -o /docker-compose https://github.com/docker/compose/releases/download/$DOCKER_COMPOSE_VERSION/docker-compose-Darwin-x86_64
RUN chmod +x /docker-compose
RUN curl -fsSL -o /boot2docker.iso https://github.com/boot2docker/boot2docker/releases/download/v$DOCKER_VERSION/boot2docker.iso
RUN curl -fsSL -o /kitematic.zip https://github.com/kitematic/kitematic/releases/download/v$KITEMATIC_VERSION/Kitematic-$KITEMATIC_VERSION-Mac.zip
#   Extract the VirtualBox .pkg
RUN mkdir -p /mpkg/vbox \
 && cd /mpkg/vbox \
 && 7z x /vbox.dmg -ir'!*.hfs' \
 && 7z x `find . -name '*.hfs' ` -ir'!*.pkg' \
 && mv VirtualBox/VirtualBox.pkg . \
 && rm -rf vbox.dmg \
 && rm -rf `find . -name '*.hfs' `
#  Extract the .pkg files
RUN cd /mpkg/vbox \
 && mv VirtualBox.pkg /tmp \
 && xar -xf /tmp/VirtualBox.pkg \
 && rm -rf /tmp/VirtualBox.pkg
RUN cd /mpkg/vbox \
 && mv *.pkg .. \
 && rm -rf vbox
#  Add components
COPY osx/mpkg/Distribution /mpkg/Distribution
#  docker.pkg
COPY osx/mpkg/docker.pkg /mpkg/docker.pkg
RUN cd /mpkg/docker.pkg \
 && mkdir rootfs \
 && cd rootfs \
 && mkdir -p usr/local/bin \
 && mv /docker usr/local/bin/ \
 && ls -al /usr/local/bin/ \
 && find . | cpio -o --format odc | gzip -c > ../Payload \
 && mkbom . ../Bom \
 && sed -i -e "s/%DOCKER_NUMBER_OF_FILES%/`find . | wc -l `/g" -e "s/%DOCKER_INSTALL_KBYTES%/`du -sk | cut -f1 `/g" -e "s/%DOCKER_VERSION%/$DOCKER_VERSION/g" ../PackageInfo /mpkg/Distribution \
 && cd .. \
 && rm -rf ./rootfs
#  kitematicapp.pkg
COPY osx/mpkg/kitematicapp.pkg /mpkg/kitematicapp.pkg
RUN cd /mpkg/kitematicapp.pkg \
 && mkdir ./rootfs \
 && cd ./rootfs \
 && 7z x /kitematic.zip \
 && rm -rf ./__MACOSX \
 && ls -al . \
 && find . | cpio -o --format odc | gzip -c > ../Payload \
 && mkbom . ../Bom \
 && sed -i -e "s/%KITEMATICAPP_NUMBER_OF_FILES%/`find . | wc -l `/g" -e "s/%KITEMATICAPP_INSTALL_KBYTES%/`du -sk | cut -f1 `/g" -e "s/%KITEMATICAPP_VERSION%/$KITEMATIC_VERSION/g" ../PackageInfo /mpkg/Distribution \
 && cd .. \
 && rm -rf ./rootfs
#  dockermachine.pkg
COPY osx/mpkg/dockermachine.pkg /mpkg/dockermachine.pkg
RUN cd /mpkg/dockermachine.pkg \
 && mkdir rootfs \
 && cd rootfs \
 && mkdir -p usr/local/bin \
 && mv /docker-machine usr/local/bin/ \
 && ls -al /usr/local/bin/ \
 && find . | cpio -o --format odc | gzip -c > ../Payload \
 && mkbom . ../Bom \
 && sed -i -e "s/%DOCKERMACHINE_NUMBER_OF_FILES%/`find . | wc -l `/g" -e "s/%DOCKERMACHINE_INSTALL_KBYTES%/`du -sk | cut -f1 `/g" -e "s/%DOCKERMACHINE_VERSION%/$DOCKER_MACHINE_VERSION/g" ../PackageInfo /mpkg/Distribution \
 && cd .. \
 && rm -rf ./rootfs
#  dockercompose.pkg
COPY osx/mpkg/dockercompose.pkg /mpkg/dockercompose.pkg
RUN cd /mpkg/dockercompose.pkg \
 && mkdir rootfs \
 && cd rootfs \
 && mkdir -p usr/local/bin \
 && mv /docker-compose usr/local/bin/ \
 && ls -al /usr/local/bin/ \
 && find . | cpio -o --format odc | gzip -c > ../Payload \
 && mkbom . ../Bom \
 && sed -i -e "s/%DOCKERCOMPOSE_NUMBER_OF_FILES%/`find . | wc -l `/g" -e "s/%DOCKERCOMPOSE_INSTALL_KBYTES%/`du -sk | cut -f1 `/g" -e "s/%DOCKERCOMPOSE_VERSION%/$DOCKER_COMPOSE_VERSION/g" ../PackageInfo /mpkg/Distribution \
 && cd .. \
 && rm -rf ./rootfs
#  boot2dockeriso.pkg
COPY osx/mpkg/boot2dockeriso.pkg /mpkg/boot2dockeriso.pkg
RUN cd /mpkg/boot2dockeriso.pkg \
 && cd Scripts \
 && find . | cpio -o --format odc | gzip -c > ../Scripts.bin \
 && cd .. \
 && rm -r Scripts \
 && mv Scripts.bin Scripts \
 && mkdir ./rootfs \
 && cd ./rootfs \
 && cp /boot2docker.iso . \
 && find . | cpio -o --format odc | gzip -c > ../Payload \
 && mkbom . ../Bom \
 && sed -i -e "s/%BOOT2DOCKER_ISO_NUMBER_OF_FILES%/`find . | wc -l `/g" -e "s/%BOOT2DOCKER_ISO_INSTALL_KBYTES%/`du -sk | cut -f1 `/g" -e "s/%DOCKER_VERSION%/$DOCKER_VERSION/g" ../PackageInfo /mpkg/Distribution \
 && cd .. \
 && rm -rf ./rootfs
#  dockerquickstartterminalapp.pkg
COPY osx/mpkg/dockerquickstartterminalapp.pkg /mpkg/dockerquickstartterminalapp.pkg
COPY osx/mpkg/quickstart.app /mpkg/quickstart.app
RUN cd /mpkg/dockerquickstartterminalapp.pkg \
 && mkdir ./rootfs \
 && cd ./rootfs \
 && mv /mpkg/quickstart.app ./Docker Quickstart Terminal.app \
 && find . | cpio -o --format odc | gzip -c > ../Payload \
 && mkbom . ../Bom \
 && sed -i -e "s/%DOCKERQUICKSTARTTERMINALAPP_NUMBER_OF_FILES%/`find . | wc -l `/g" -e "s/%DOCKERQUICKSTARTTERMINALAPP_INSTALL_KBYTES%/`du -sk | cut -f1 `/g" -e "s/%DOCKERQUICKSTARTTERMINALAPP_VERSION%/$INSTALLER_VERSION/g" ../PackageInfo /mpkg/Distribution \
 && cd .. \
 && rm -rf ./rootfs
COPY osx/mpkg/Resources /mpkg/Resources
COPY osx/mpkg/Plugins /mpkg/Plugins
RUN sed -i -e "s/%MIXPANEL_TOKEN%/$MIXPANEL_TOKEN/g" -e "s/%INSTALLER_VERSION%/$INSTALLER_VERSION/g" mpkg/Plugins/*.bundle/Contents/Info.plist
RUN sed -i -e "s/%INSTALLER_VERSION%/$INSTALLER_VERSION/g" mpkg/Plugins/*.bundle/Contents/Resources/*.html
RUN sed -i -e "s/%INSTALLER_VERSION%/$INSTALLER_VERSION/g" mpkg/Resources/en.lproj/welcome.rtfd/TXT.rtf
RUN sed -i -e "s/%VBOX_VERSION%/$VBOX_VERSION/g" /mpkg/Distribution \
 && sed -i -e "s/%VBOX_VERSION%/$VBOX_VERSION/g" -e "s/%DOCKER_VERSION%/$DOCKER_VERSION/g" -e "s/%DOCKERMACHINE_VERSION%/$DOCKER_MACHINE_VERSION/g" -e "s/%DOCKERCOMPOSE_VERSION%/$DOCKER_COMPOSE_VERSION/g" -e "s/%DOCKER_VERSION%/$DOCKER_VERSION/g" -e "s/%DOCKERQUICKSTARTTERMINALAPP_VERSION%/$INSTALLER_VERSION/g" -e "s/%KITEMATICAPP_VERSION%/$KITEMATIC_VERSION/g" mpkg/Resources/en.lproj/Localizable.strings
#  Repackage back. Yes, --compression=none is mandatory.
#  or this won't install in OSX.
RUN cd /mpkg \
 && xar -c --compression=none -f /DockerToolbox.pkg .
