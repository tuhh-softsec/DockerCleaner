#   Grab a snapshot of the `alpine` docker image from September 19, 2016.
FROM alpine@sha256:02eb5cfe4b721495135728ab4aea87418fd4edbfbf83612130a81191f0b2aae3
#   Add a partial local mirror of the alpine latest repository, signed
#   by the alpine developers.
COPY apks /apks
RUN echo "/apks/" > /etc/apk/repositories
#   Install some utilities that we need.
RUN apk update
RUN apk add alpine-sdk=1.0-r1
RUN apk add nano=7.0-r0
RUN apk add xorriso=1.5.4-r2
RUN apk add syslinux=6.04_pre1-r11
#   Add the 'builder' user (to the 'abuild' group)
RUN echo "builder:x:1000:100::/home/builder:/bin/bash" >> /etc/passwd
RUN mkdir /home/builder
RUN chown builder:abuild /home/builder
RUN sed -i.bak '/abuild/d' /etc/group
RUN echo "abuild:x:300:builder" >> /etc/group
#   Add the APK keys to satisfy alpine's tooling. We do not
#   rely on these keys for authentication or a source of trust;
#   they are public.
RUN mkdir /home/builder/.abuild/
COPY iso.rsa /home/builder/.abuild/iso.rsa
RUN chmod 550 /home/builder/.abuild/iso.rsa
COPY iso.rsa.pub /home/builder/.abuild/iso.rsa.pub
RUN echo 'PACKAGER_PRIVKEY="/home/builder/.abuild/iso.rsa"' > /home/builder/.abuild/abuild.conf
RUN chown -R builder:abuild /home/builder/.abuild
RUN mkdir /root/.abuild/
COPY iso.rsa /root/.abuild/iso.rsa
RUN chmod 550 /root/.abuild/iso.rsa
COPY iso.rsa.pub /root/.abuild/iso.rsa.pub
RUN echo 'PACKAGER_PRIVKEY="/root/.abuild/iso.rsa"' > /root/.abuild/abuild.conf
RUN chown -R builder:abuild /root/.abuild
COPY iso.rsa.pub /etc/apk/keys/iso.rsa.pub
#   Add the `mpc_compute` package.
COPY mpc_compute /home/builder/mpc_compute/
RUN chown -R builder:abuild /home/builder/mpc_compute/
#   Add the `mpc_network` package
COPY mpc_network /home/builder/mpc_network/
RUN chown -R builder:abuild /home/builder/mpc_network/
#   Add the `gradm-improved` package
COPY gradm-improved /home/builder/gradm-improved/
RUN chown -R builder:abuild /home/builder/gradm-improved/
#   Run some setup as `builder`
USER builder
#   Build the "gradm-improved" package.
WORKDIR /home/builder/gradm-improved/
RUN abuild checksum
RUN abuild -r
RUN cp -r /home/builder/packages/builder /home/builder/apks_custom_gradm/
RUN rm -rf /home/builder/packages
USER root
RUN echo "/home/builder/apks_custom_gradm/" >> /etc/apk/repositories
RUN apk update
USER builder
#   Build the MPC "compute" package that runs at boot.
WORKDIR /home/builder/mpc_compute/
RUN abuild checksum
RUN abuild -r
#   Build the MPC "network" package that runs at boot.
RUN rm /home/builder/packages/builder/x86_64/APKINDEX.tar.gz
WORKDIR /home/builder/mpc_network/
RUN abuild checksum
RUN abuild -r
#   Move built packages into apks_custom.
RUN cp -r /home/builder/packages/builder /home/builder/apks_custom/
#   Add the custom apks repository
USER root
RUN echo "/home/builder/apks_custom/" >> /etc/apk/repositories
RUN apk update
USER builder
#   Grab the `alpine-iso` package.
WORKDIR /home/builder/
COPY alpine-iso /home/builder/alpine-iso/
WORKDIR /home/builder/alpine-iso/
#   Grab the configurations for the ISOs
COPY alpine-compute.conf.mk /home/builder/alpine-iso/alpine-compute.conf.mk
COPY alpine-compute.packages /home/builder/alpine-iso/alpine-compute.packages
COPY alpine-network.conf.mk /home/builder/alpine-iso/alpine-network.conf.mk
COPY alpine-network.packages /home/builder/alpine-iso/alpine-network.packages
#   Build the ISOs
USER root
RUN make PROFILE=alpine-compute iso
RUN make PROFILE=alpine-network iso
RUN mkdir -p /home/builder/target/
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
