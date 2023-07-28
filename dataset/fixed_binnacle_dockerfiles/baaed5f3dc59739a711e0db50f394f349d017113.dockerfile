FROM alpine:3.8 AS build
RUN apk add cmake=3.11.1-r2 boost-dev=1.66.0-r0 make=4.2.1-r2 curl=7.61.1-r3 git=2.18.4-r0 curl-dev=7.61.1-r3 ruby=2.5.8-r0 ruby-dev=2.5.8-r0 gcc=6.4.0-r9 g++=6.4.0-r9 yaml-cpp-dev=0.6.2-r1 jq=1.6_rc1-r1 openjdk8=8.275.01-r0 --no-cache
ENV JAVA_HOME="/usr/lib/jvm/java-1.8-openjdk"
ENV PATH="$PATH:$JAVA_HOME/jre/bin:$JAVA_HOME/bin"
SHELL ["/bin/ash", "-eo", "pipefail", "-c"]
RUN mkdir /workspace
WORKDIR /workspace
RUN sed -i -e 's/sys\/poll/poll/' /usr/include/boost/asio/detail/socket_types.hpp
COPY configs/components/leatherman.json /workspace
RUN git clone -b 1.5.x https://github.com/puppetlabs/leatherman \
 && mkdir -p /workspace/leatherman/build
WORKDIR /workspace/leatherman/build
RUN git checkout "$( jq .ref /workspace/leatherman.json | tr -d \" ;)" \
 && cmake .. -DBOOST_STATIC=OFF -DCMAKE_VERBOSE_MAKEFILE=ON ; make ; make install
ENV CMAKE_SHARED_OPTIONS="-DCMAKE_PREFIX_PATH=/opt/puppetlabs/puppet -DCMAKE_INSTALL_PREFIX=/opt/puppetlabs/puppet -DCMAKE_INSTALL_RPATH=/opt/puppetlabs/puppet/lib -DCMAKE_VERBOSE_MAKEFILE=ON"
WORKDIR /workspace
COPY configs/components/libwhereami.json /workspace
RUN git clone https://github.com/puppetlabs/libwhereami \
 && mkdir -p /workspace/libwhereami/build
WORKDIR /workspace/libwhereami/build
RUN git checkout "$( jq .ref /workspace/libwhereami.json | tr -d \" ;)"
RUN cmake $CMAKE_SHARED_OPTIONS -DBOOST_STATIC=OFF .. ; make ; make install
WORKDIR /workspace
COPY configs/components/cpp-hocon.json /workspace
RUN git clone https://github.com/puppetlabs/cpp-hocon \
 && mkdir -p /workspace/cpp-hocon/build
WORKDIR /workspace/cpp-hocon/build
RUN git checkout "$( jq .ref /workspace/cpp-hocon.json | tr -d \" ;)"
RUN cmake $CMAKE_SHARED_OPTIONS -DBOOST_STATIC=OFF .. ; make ; make install
WORKDIR /workspace
COPY configs/components/facter.json /workspace
RUN git clone https://github.com/puppetlabs/facter \
 && mkdir -p /workspace/facter/build
WORKDIR /workspace/facter/build
RUN git checkout "$( jq .ref /workspace/facter.json | tr -d \" ;)"
RUN cmake /lib $CMAKE_SHARED_OPTIONS -DRUBY_LIB_INSTALL=/usr/lib/ruby/vendor_ruby .. ; make ; make install
WORKDIR /workspace
COPY configs/components/cpp-pcp-client.json /workspace
RUN git clone https://github.com/puppetlabs/cpp-pcp-client \
 && mkdir -p /workspace/cpp-pcp-client/build
WORKDIR /workspace/cpp-pcp-client/build
RUN git checkout "$( jq .ref /workspace/cpp-pcp-client.json | tr -d \" ;)"
RUN cmake .. $CMAKE_SHARED_OPTIONS ; make ; make install
WORKDIR /workspace
COPY configs/components/pxp-agent.json /workspace
RUN git clone https://github.com/puppetlabs/pxp-agent \
 && mkdir -p /workspace/pxp-agent/build
WORKDIR /workspace/pxp-agent/build
RUN git checkout "$( jq .ref /workspace/pxp-agent.json | tr -d \" ;)"
RUN cmake .. $CMAKE_SHARED_OPTIONS ; make ; make install
RUN apk add augeas=1.10.1-r3 ruby-augeas=0.5.0-r4 libressl-dev=2.7.5-r0 --no-cache
RUN gem install deep_merge --version 1.2.2 --no-rdoc --no-ri
WORKDIR /workspace
RUN curl -O -L https://people.redhat.com/~rjones/virt-what/files/virt-what-1.18.tar.gz \
 && tar zxf virt-what-1.18.tar.gz
WORKDIR /workspace/virt-what-1.18
RUN ./configure \
 && make \
 && make install
WORKDIR /workspace
COPY configs/components/hiera.json /workspace
RUN git clone https://github.com/puppetlabs/hiera
WORKDIR /workspace/hiera
RUN git checkout "$( jq .ref /workspace/hiera.json | tr -d \" ;)"
RUN ./install.rb --no-configs --bindir=/opt/puppetlabs/puppet/bin --sitelibdir=/usr/lib/ruby/vendor_ruby
WORKDIR /workspace
COPY configs/components/puppet.json /workspace
RUN git clone https://github.com/puppetlabs/puppet
WORKDIR /workspace/puppet
RUN git checkout "$( jq .ref /workspace/puppet.json | tr -d \" ;)"
RUN ./install.rb --bindir=/opt/puppetlabs/puppet/bin --configdir=/etc/puppetlabs/puppet --sitelibdir=/usr/lib/ruby/vendor_ruby --codedir=/etc/puppetlabs/code --vardir=/opt/puppetlabs/puppet/cache --logdir=/var/log/puppetlabs/puppet --rundir=/var/run/puppetlabs --quick
RUN mkdir -p /opt/puppetlabs/bin \
 && ln -s /opt/puppetlabs/puppet/bin/facter /opt/puppetlabs/bin/facter \
 && ln -s /opt/puppetlabs/puppet/bin/puppet /opt/puppetlabs/bin/puppet \
 && ln -s /opt/puppetlabs/puppet/bin/hiera /opt/puppetlabs/bin/hiera
ENV PATH="/opt/puppetlabs/bin:$PATH"
RUN puppet config set confdir /etc/puppetlabs/puppet \
 && puppet config set codedir /etc/puppetlabs/code \
 && puppet config set vardir /opt/puppetlabs/puppet/cache \
 && puppet config set logdir /var/log/puppetlabs/puppet \
 && puppet config set rundir /var/run/puppetlabs
RUN mkdir -p /etc/puppetlabs/code/environment/production \
 && puppet module install puppetlabs-augeas_core \
 && puppet module install puppetlabs-cron_core \
 && puppet module install puppetlabs-host_core \
 && puppet module install puppetlabs-mount_core \
 && puppet module install puppetlabs-scheduled_task \
 && puppet module install puppetlabs-selinux_core \
 && puppet module install puppetlabs-sshkeys_core \
 && puppet module install puppetlabs-yumrepo_core \
 && puppet module install puppetlabs-zfs_core \
 && puppet module install puppetlabs-zone_core \
 && puppet module install puppetlabs-apk
FROM alpine:3.8
ARG version=6.0.0
ARG vcs_ref
ARG build_date
ENV PUPPET_AGENT_VERSION="$version"
ENV PATH="/opt/puppetlabs/bin:$PATH"
LABEL org.label-schema.maintainer="Puppet Release Team <release@puppet.com>" \
      org.label-schema.vendor="Puppet" \
      org.label-schema.url="https://github.com/puppetlabs/puppet-agent" \
      org.label-schema.name="Puppet Agent (Alpine)" \
      org.label-schema.license="Apache-2.0" \
      org.label-schema.version="$PUPPET_AGENT_VERSION" \
      org.label-schema.vcs-url="https://github.com/puppetlabs/puppet-agent" \
      org.label-schema.vcs-ref="$vcs_ref" \
      org.label-schema.build-date="$build_date" \
      org.label-schema.schema-version="1.0" \
      org.label-schema.dockerfile="/Dockerfile"
RUN apk add bash=4.4.19-r1 ruby=2.5.8-r0 bind-tools=9.12.4_p2-r0 curl=7.61.1-r3 shadow=4.5-r0 yaml-cpp=0.6.2-r1 ruby-json=2.5.8-r0 ruby-augeas=0.5.0-r4 boost-random=1.66.0-r0 boost-iostreams=1.66.0-r0 boost-graph=1.66.0-r0 boost-signals=1.66.0-r0 boost=1.66.0-r0 boost-serialization=1.66.0-r0 boost-program_options=1.66.0-r0 boost-system=1.66.0-r0 boost-unit_test_framework=1.66.0-r0 boost-math=1.66.0-r0 boost-doc=1.66.0-r0 boost-wserialization=1.66.0-r0 boost-date_time=1.66.0-r0 boost-wave=1.66.0-r0 boost-filesystem=1.66.0-r0 boost-prg_exec_monitor=1.66.0-r0 boost-regex=1.66.0-r0 boost-thread=1.66.0-r0 --no-cache
COPY --from=build /usr/lib/ruby/vendor_ruby /usr/lib/ruby/vendor_ruby
COPY --from=build /etc/puppetlabs /etc/puppetlabs
COPY --from=build /opt/puppetlabs /opt/puppetlabs
COPY --from=build /usr/lib/ruby/gems /usr/lib/ruby/gems
COPY --from=build /usr/lib/ruby/vendor_ruby /usr/lib/ruby/vendor_ruby
ENTRYPOINT ["/opt/puppetlabs/bin/puppet"]
CMD ["agent", "--verbose", "--onetime", "--no-daemonize", "--summarize"]
COPY docker/puppet-agent-alpine/Dockerfile /
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
