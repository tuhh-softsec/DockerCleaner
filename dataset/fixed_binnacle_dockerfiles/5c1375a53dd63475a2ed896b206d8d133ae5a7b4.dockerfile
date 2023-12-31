FROM openshifttools/oso-centos7-ops-base:latest
#       ___ ___ _  _ ___ ___    _ _____ ___ ___         
#      / __| __| \| | __| _ \  /_\_   _| __|   \        
#     | (_ | _|| .` | _||   / / _ \| | | _|| |) |       
#      \___|___|_|\_|___|_|_\/_/_\_\_|_|___|___/_ _____ 
#     |   \ / _ \  | \| |/ _ \_   _| | __|   \_ _|_   _|
#     | |) | (_) | | .` | (_) || |   | _|| |) | |  | |  
#     |___/ \___/  |_|\_|\___/ |_|   |___|___/___| |_|  
#   
#   Pause indefinitely if asked to do so.
ARG OO_PAUSE_ON_BUILD
RUN test "$OO_PAUSE_ON_BUILD" = "true" \
 && while sleep 10 ; do true ; done || :
#   PCP
#  #################
#   install pcp-collector and it's dependencies, clean the cache.
RUN yum-install-check.sh -y pcp pcp-conf pcp-collector xz \
 && yum clean all
#   Run in the container as root - avoids PCP_USER mismatches
RUN sed -i -e 's/PCP_USER=.*$/PCP_USER=root/' -e 's/PCP_GROUP=.*$/PCP_GROUP=root/' /etc/pcp.conf
#   Disable service advertising - no avahi daemon in the container
#   (dodges warnings from pmcd attempting to connect during startup)
RUN . /etc/pcp.conf \
 && echo "-A" >> $PCP_PMCDOPTIONS_PATH
#   denote this as a container environment, for rc scripts
ENV PCP_CONTAINER_IMAGE="pcp-collector"
ENV NAME="pcp-collector"
ENV IMAGE="pcp-collector"
ENV PATH="/usr/share/pcp/lib:/usr/libexec/pcp/bin:$PATH"
#   script to watch health of pmcd
COPY check-pmcd-status.sh /usr/local/bin/check-pmcd-status.sh
#  #################
#   Add google-cloud-sdk repo
COPY google-cloud-sdk.repo /etc/yum.repos.d/
#   Add copr repo for python-hawkular-client rpm
RUN cd /etc/yum.repos.d \
 && curl -O https://copr.fedorainfracloud.org/coprs/g/Hawkular/python-hawkular-client/repo/epel-7/group_Hawkular-python-hawkular-client-epel-7.repo
RUN yum clean metadata \
 && yum-install-check.sh -y python2-pip pcp pcp-conf pcp-testsuite python-requests pyOpenSSL python-openshift-tools python-openshift-tools-monitoring-pcp python-openshift-tools-monitoring-docker python-openshift-tools-monitoring-zagg python-openshift-tools-monitoring-openshift python-openshift-tools-ansible python-openshift-tools-web openshift-tools-scripts-cloud-aws openshift-tools-scripts-cloud-gcp openshift-tools-scripts-monitoring-pcp openshift-tools-scripts-monitoring-docker openshift-tools-scripts-monitoring-aws openshift-tools-scripts-monitoring-gcp openshift-tools-scripts-monitoring-openshift openshift-tools-scripts-monitoring-autoheal pcp-manager pcp-webapi python-pcp python-httplib2 python2-pyasn1 python2-pyasn1-modules python2-rsa python-configobj python2-psutil pylint tito python-devel libyaml-devel oso-simplesamlphp python2-ruamel-yaml rpm-sign createrepo python2-boto3 python-lxml rkhunter python-hawkular-client python-docker \
 && yum clean all
COPY urllib3-connectionpool-patch /root/
RUN yum-install-check.sh -y patch \
 && yum clean all \
 && cd /usr/lib/python2.7/site-packages/ \
 && patch -p1 < /root/urllib3-connectionpool-patch
#   make mount points for security update count check, and make a circular symlink because yum is dumb about its root
RUN mkdir -p /host /var/local/hostpkg/etc/rhsm/ca /var/local/hostpkg/etc/rpm /var/local/hostpkg/etc/pki/entitlement /var/local/hostpkg/etc/pki/rpm-gpg /var/local/hostpkg/var/local /var/local/hostpkg/var/cache/yum /var/local/hostpkg/var/lib/yum \
 && ln -s /var/local/hostpkg /var/local/hostpkg/var/local/hostpkg
#   Make mount points for rkhunter files, and configure rkhunter to work with this structure
RUN mkdir -p /var/local/rkhunter_chroot /var/local/rkhunter_tmp /var/local/rkhunter_tmp/rkhunter /var/local/rkhunter_tmp/rkhunter/bin /var/local/rkhunter_tmp/rkhunter/db /var/local/rkhunter_tmp/rkhunter/etc /var/local/rkhunter_tmp/rkhunter/scripts \
 && sed -i 's/\/var\/log\/rkhunter\/rkhunter.log/\/var\/local\/rkhunter_tmp\/rkhunter\/rkhunter.log/' /etc/logrotate.d/rkhunter \
 && sed -r -e 's%^(SCRIPTDIR)=.*%\1=/tmp/rkhunter/scripts%; s%^(LOGFILE)=.*%\1=/tmp/rkhunter/rkhunter.log%' /etc/rkhunter.conf > /var/local/rkhunter_tmp/rkhunter/etc/rkhunter.conf
#   Ansible startup configuration playbook
COPY root /root
RUN cat bash_aliases >> /root/.bashrc
#   FIXME: These are vendor libs that need to be packaged and installed via RPM.
COPY vendor/prometheus_client /usr/lib/python2.7/site-packages/prometheus_client/
#   Create ops-runner.log file with proper permissions
RUN touch /var/log/ops-runner.log \
 && chmod 664 /var/log/ops-runner.log
#   Setup the AWS credentials file so that we can populate it on startup.
RUN mkdir -p /root/.aws \
 && touch /root/.aws/credentials \
 && chmod g+rw /root/.aws/credentials
#   Add container-build-env-fingerprint
COPY container-build-env-fingerprint.output /etc/oso-container-build-env-fingerprint
#   Add the start script and tell the container to run it by default
COPY start.sh /usr/local/bin/
CMD /usr/local/bin/start.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
