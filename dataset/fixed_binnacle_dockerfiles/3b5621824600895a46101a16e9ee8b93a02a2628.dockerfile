FROM ubuntu:16.04
EXPOSE 80/tcp 5000/tcp 8773/tcp 8774/tcp 8775/tcp 8776/tcp 9292/tcp
#   Suppress unwanted debconf messages and questions during build
ARG DEBIAN_FRONTEND=noninteractive
#  ####################################################################
#   Systemd workaround from solita/ubuntu-systemd and moby/moby#28614 #
#  ####################################################################
ENV container="docker"
#   No need for graphical.target
RUN systemctl set-default multi-user.target
#   Gracefully stop systemd
STOPSIGNAL SIGRTMIN+3
#   Cleanup unneeded services
RUN find /etc/systemd/system /lib/systemd/system -path '*.wants/*' -not -name '*journald*' -not -name '*systemd-tmpfiles*' -not -name '*systemd-user-sessions*' -exec rm {}
#   Workaround for console output error moby/moby#27202, based on moby/moby#9212
CMD ["/bin/bash", "-c", "exec", "/sbin/init", "--log-target=journal", "3>&1"]
#  ###################
#   DevStack Preload #
#  ###################
#   Get Missing External System Dependencies for DevStack Setup
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20210119~16.04.1 git=1:2.7.4-0ubuntu1.10 iproute2=4.3.0-1ubuntu3.16.04.5 liberasurecode-dev=1.1.0-3 libnss3-dev=2:3.28.4-0ubuntu0.16.04.14 libsystemd-dev=229-4ubuntu21.31 libvirt-dev=1.3.1-1ubuntu10.31 lsb=9.20160110ubuntu0.2 net-tools=1.60-26ubuntu1 python-virtualenv=15.0.1+ds-3ubuntu1.1 software-properties-common=0.96.20.10 --assume-yes \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
ARG DEVSTACK_BRANCH="master"
ARG PROJECTS_BRANCH="master"
#   This OpenStack project repositories will be downloaded
ARG PROJECTS="  keystone  nova  neutron  glance  horizon  zun  zun-ui  kuryr-libnetwork  "
#   Clone DevStack, Requirements and OpenStack (Core) Projects
#    - To properly detect a container environment,
#      we need at least openstack-dev/devstack/commit/63666a2
RUN git clone git://git.openstack.org/openstack-dev/devstack --branch $DEVSTACK_BRANCH \
 && git clone git://git.openstack.org/openstack/requirements --branch $DEVSTACK_BRANCH /opt/stack/requirements \
 && for PROJECT in $PROJECTS; do git clone git://git.openstack.org/openstack/$PROJECT.git /opt/stack/$PROJECT --branch $PROJECTS_BRANCH --depth 1 --single-branch ; done
#   Pre-Install DevStack System Dependencies to Speedup Docker Run
RUN /devstack/tools/install_prereqs.sh \
 && echo 'mysql-server mysql-server/root_password password secret' | debconf-set-selections \
 && echo 'mysql-server mysql-server/root_password_again password secret' | debconf-set-selections \
 && apt-get update \
 && apt-get install --no-install-recommends mysql-server=5.7.33-0ubuntu0.16.04.1 rabbitmq-server=3.5.7-1ubuntu0.16.04.4 --assume-yes \
 && service mysql start \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Install external pip, resolving missing python-setuptools and wheel errors
RUN curl --silent --show-error https://bootstrap.pypa.io/get-pip.py | python
#   Solve dependency conflict for urllib3 with pre-installed requests
RUN pip install requests==2.28.2 --upgrade --force-reinstall
#   Install known working Python packages
RUN pip install /opt/stack/requirements/upper-constraints.txt --no-cache-dir --constraint --requirement /opt/stack/requirements/global-requirements.txt --requirement /opt/stack/requirements/test-requirements.txt
#   Setup non-Root user "stack", as required by stack.sh
RUN useradd --shell /bin/bash --home-dir /opt/stack/ stack \
 && echo "stack ALL=(ALL) NOPASSWD: ALL" | tee /etc/sudoers.d/stack \
 && sudo chown --recursive stack /devstack/
#   Copy DevStack configuration, if file has changed
COPY local.conf /devstack/
#   This container starts systemd, so do not add an additional Docker CMD!
#   Place any post-start calls in the Makefile.
#   Actual DevStack setup has to happen in a running container,
#   because of missing privileges during Docker build.
#   Thats's right, we add ARGs and dependent labels at the end, because they will change on every build
ARG BUILD_DATE
ARG VCS_REF
ARG VERSION
LABEL org.label-schema.build-date="$BUILD_DATE" \
      org.label-schema.name="DockStack" \
      org.label-schema.description="Docker on DevStack on Docker" \
      org.label-schema.version="$VERSION-$BUILD_DATE-git-$VCS_REF" \
      org.label-schema.vendor="Jan Mattfeld" \
      org.label-schema.vcs-url="https://github.com/janmattfeld/DockStack" \
      org.label-schema.vcs-ref="$VCS_REF" \
      org.label-schema.docker.cmd="docker run --privileged --detach devstack " \
      org.label-schema.docker.params="DEVSTACK_BRANCH, PROJECTS_BRANCH, PROJECTS" \
      org.label-schema.schema-version="1.0"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
