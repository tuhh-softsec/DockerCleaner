FROM centos:7
MAINTAINER Humble Chirammal hchiramm@redhat.com Saravanakumar Arumugam sarumuga@redhat.com
ENV container="docker"
ENV ARCH="\"x86_64\""
LABEL architecture="$ARCH" \
      name="gluster/gluster-centos" \
      version="latest" \
      vendor="CentOS Community" \
      summary="This image has a running glusterfs service (CentOS 7 + latest Gluster)" \
      io.k8s.display-name="Gluster server based on CentOS 7" \
      io.k8s.description="Gluster Image is based on CentOS Image which is a scalable network filesystem. Using common off-the-shelf hardware, you can create large, distributed storage solutions for media streaming, data analysis, and other data- and bandwidth-intensive tasks." \
      description="Gluster Image is based on CentOS Image which is a scalable network filesystem. Using common off-the-shelf hardware, you can create large, distributed storage solutions for media streaming, data analysis, and other data- and bandwidth-intensive tasks." \
      io.openshift.tags="gluster,glusterfs,glusterfs-centos"
RUN yum --setopt=tsflags=nodocs -y update \
 && yum install -y centos-release-gluster \
 && yum clean all \
 && (cd /lib/systemd/system/sysinit.target.wants/ ;for i in *; do [ $i == systemd-tmpfiles-setup.service ] || rm -f $i ; done ) \
 && rm -f /lib/systemd/system/multi-user.target.wants/* \
 && rm -f /etc/systemd/system/*.wants/* \
 && rm -f /lib/systemd/system/local-fs.target.wants/* \
 && rm -f /lib/systemd/system/sockets.target.wants/*udev* \
 && rm -f /lib/systemd/system/sockets.target.wants/*initctl* \
 && rm -f /lib/systemd/system/basic.target.wants/* \
 && rm -f /lib/systemd/system/anaconda.target.wants/* \
 && yum --setopt=tsflags=nodocs -y install nfs-utils \
 && yum --setopt=tsflags=nodocs -y install attr \
 && yum --setopt=tsflags=nodocs -y install iputils \
 && yum --setopt=tsflags=nodocs -y install iproute \
 && yum --setopt=tsflags=nodocs -y install openssh-server \
 && yum --setopt=tsflags=nodocs -y install openssh-clients \
 && yum --setopt=tsflags=nodocs -y install rsync \
 && yum --setopt=tsflags=nodocs -y install tar \
 && yum --setopt=tsflags=nodocs -y install cronie \
 && yum --setopt=tsflags=nodocs -y install sudo \
 && yum --setopt=tsflags=nodocs -y install xfsprogs \
 && yum --setopt=tsflags=nodocs -y install glusterfs \
 && yum --setopt=tsflags=nodocs -y install glusterfs-server \
 && yum --setopt=tsflags=nodocs -y install glusterfs-rdma \
 && yum --setopt=tsflags=nodocs -y install gluster-block \
 && yum --setopt=tsflags=nodocs -y install glusterfs-geo-replication \
 && yum clean all \
 && sed -i '/Defaults requiretty/c\#Defaults requiretty' /etc/sudoers \
 && sed -i '/Port 22/c\Port 2222' /etc/ssh/sshd_config \
 && sed -i 's/Requires\=rpcbind\.service//g' /usr/lib/systemd/system/glusterd.service \
 && sed -i 's/rpcbind\.service/gluster-setup\.service/g' /usr/lib/systemd/system/glusterd.service \
 && sed -i 's/rpcbind\.service//g' /usr/lib/systemd/system/gluster-blockd.service \
 && mkdir -p /etc/glusterfs_bkp /var/lib/glusterd_bkp /var/log/glusterfs_bkp \
 && cp -r /etc/glusterfs/* /etc/glusterfs_bkp \
 && cp -r /var/lib/glusterd/* /var/lib/glusterd_bkp \
 && cp -r /var/log/glusterfs/* /var/log/glusterfs_bkp \
 && mkdir -p /var/log/core
#  downgrade lvm2 because of regression reported in https://bugzilla.redhat.com/1676612
#  once the bug is fixed, disable obtain_device_list_from_udev in lvm.conf
RUN true \
 && yum -y downgrade device-mapper-libs-1.02.149-10.el7_6.2 device-mapper-1.02.149-10.el7_6.2 device-mapper-event-libs-1.02.149-10.el7_6.2 device-mapper-event-1.02.149-10.el7_6.2 device-mapper-persistent-data-0.7.0-0.1.rc6.el7_4.1 lvm2-libs-2.02.180-10.el7_6.2 lvm2-2.02.180-10.el7_6.2 \
 && yum -y clean all \
 && true
#  do not run udev (if needed, bind-mount /run/udev instead?)
RUN true \
 && systemctl mask systemd-udev-trigger.service \
 && systemctl mask systemd-udevd.service \
 && systemctl mask systemd-udevd.socket \
 && systemctl mask systemd-udevd-kernel.socket \
 && true
#  use lvmetad from the host, dont run it in the container
#  don't wait for udev to manage the /dev entries, disable udev_sync, udev_rules in lvm.conf
VOLUME [ "/run/lvm" ]
RUN true \
 && systemctl mask lvm2-lvmetad.service \
 && systemctl mask lvm2-lvmetad.socket \
 && sed -i 's/^\sudev_rules\s*=\s*1/udev_rules = 0/' /etc/lvm/lvm.conf \
 && sed -i 's/^\sudev_sync\s*=\s*1/udev_sync= 0/' /etc/lvm/lvm.conf \
 && sed -i 's/^\sobtain_device_list_from_udev\s*=\s*1/obtain_device_list_from_udev = 0/' /etc/lvm/lvm.conf \
 && true
#  prevent dmeventd from running in the container, it may cause conflicts with
#  the service running on the host
#  monitoring of activated LVs can not be done inside the container
RUN true \
 && systemctl mask dm-event.service \
 && systemctl disable dm-event.socket \
 && systemctl mask dm-event.socket \
 && systemctl disable lvm2-monitor.service \
 && systemctl mask lvm2-monitor.service \
 && sed -i 's/^\smonitoring\s*=\s*1/monitoring = 0/' /etc/lvm/lvm.conf \
 && true
VOLUME [ "/sys/fs/cgroup" ]
COPY gluster-fake-disk.service /etc/systemd/system/gluster-fake-disk.service
COPY fake-disk.sh /usr/libexec/gluster/fake-disk.sh
COPY gluster-setup.service /etc/systemd/system/gluster-setup.service
COPY gluster-setup.sh /usr/sbin/gluster-setup.sh
COPY gluster-block-setup.service /etc/systemd/system/gluster-block-setup.service
COPY gluster-block-setup.sh /usr/sbin/gluster-block-setup.sh
COPY update-params.sh /usr/local/bin/update-params.sh
COPY status-probe.sh /usr/local/bin/status-probe.sh
COPY tcmu-runner-params /etc/sysconfig/tcmu-runner-params
COPY gluster-check-diskspace.service /etc/systemd/system/gluster-check-diskspace.service
COPY check_diskspace.sh /usr/local/bin/check_diskspace.sh
RUN chmod 644 /etc/systemd/system/gluster-setup.service \
 && chmod 644 /etc/systemd/system/gluster-check-diskspace.service \
 && chmod 755 /usr/libexec/gluster/fake-disk.sh \
 && chmod 500 /usr/sbin/gluster-setup.sh \
 && chmod 644 /etc/systemd/system/gluster-block-setup.service \
 && chmod 500 /usr/sbin/gluster-block-setup.sh \
 && chmod +x /usr/local/bin/update-params.sh \
 && chmod +x /usr/local/bin/status-probe.sh \
 && chmod +x /usr/local/bin/check_diskspace.sh \
 && systemctl disable nfs-server.service \
 && systemctl mask getty.target \
 && systemctl enable gluster-fake-disk.service \
 && systemctl enable gluster-setup.service \
 && systemctl enable gluster-block-setup.service \
 && systemctl enable gluster-blockd.service \
 && systemctl enable glusterd.service \
 && systemctl enable gluster-check-diskspace.service
EXPOSE 2222/tcp 111/tcp 245/tcp 443/tcp 24007/tcp 2049/tcp 8080/tcp 6010/tcp 6011/tcp 6012/tcp 38465/tcp 38466/tcp 38468/tcp 38469/tcp 49152/tcp 49153/tcp 49154/tcp 49156/tcp 49157/tcp 49158/tcp 49159/tcp 49160/tcp 49161/tcp 49162/tcp
ENTRYPOINT ["/usr/local/bin/update-params.sh"]
CMD ["/usr/sbin/init"]
