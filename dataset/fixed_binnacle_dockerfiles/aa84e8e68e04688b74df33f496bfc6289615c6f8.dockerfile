FROM reggaemuffin/steem-baseimage:latest
ENV LANG="en_US.UTF-8"
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf=2.71-3 automake=1:1.16.5-1.3 autotools-dev=20220109.1 bsdmainutils=12.1.7+nmu3ubuntu2 build-essential=12.9ubuntu3 cmake=3.25.1-1 doxygen=1.9.4-4 gdb=13.1-2ubuntu2 git=1:2.39.2-1ubuntu1 libboost-all-dev=1.74.0.3ubuntu7 libyajl-dev=2.1.0-3build2 libreadline-dev=8.2-1.3 libssl-dev=3.0.8-1ubuntu1 libtool=2.4.7-5 liblz4-tool=1.9.4-1 ncurses-dev pkg-config=1.8.1-1ubuntu2 python3=3.11.2-1 python3-dev=3.11.2-1 python3-jinja2=3.1.2-1 python3-pip=23.0.1+dfsg-1 nginx=1.22.0-1ubuntu3 fcgiwrap=1.1.0-14 awscli=2.9.19-1 jq=1.6-2.1ubuntu3 wget=1.21.3-1ubuntu1 virtualenv=20.19.0+ds-1 gdb=13.1-2ubuntu2 libgflags-dev=2.2.2-2 libsnappy-dev=1.1.9-3 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 libbz2-dev=1.0.8-5build1 liblz4-dev=1.9.4-1 libzstd-dev=1.5.4+dfsg2-4 lcov=1.16-1 ruby=1:3.1 ccache=4.7.4-1 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && pip3 install gcovr \
 && gem install mtime_cache --version 1.0.2
COPY . /usr/local/src/steem
RUN cd /usr/local/src/steem \
 && git submodule update --init --recursive \
 && sh ciscripts/compiletest.sh \
 && cd /usr/local/src/steem/build \
 && ./tests/chain_test \
 && ./tests/plugin_test \
 && cd .. \
 && sh ./ciscripts/collectcoverage.sh \
 && cd build \
 && ./programs/util/test_fixed_string \
 && cd /usr/local/src/steem \
 && programs/build_helpers/get_config_check.sh \
 && doxygen \
 && PYTHONPATH=programs/build_helpers python3 -m steem_build_helpers.check_reflect \
 && mkdir -p /usr/local/src/steemtmp \
 && mv /usr/local/src/steem/build /usr/local/src/steemtmp \
 && mv /usr/local/src/steem/.mtime_cache /usr/local/src/steemtmp \
 && mv /usr/local/src/steem/.ccache /usr/local/src/steemtmp \
 && mv /usr/local/src/steem/coverage.info /usr/local/src/steemtmp \
 && rm -rf /usr/local/src/steem
RUN apt-get remove -y autoconf automake autotools-dev bsdmainutils build-essential cmake doxygen gdb git libboost-all-dev libyajl-dev libreadline-dev libssl-dev libtool liblz4-tool ncurses-dev pkg-config python3 python3-dev python3-jinja2 python3-pip nginx fcgiwrap awscli jq wget virtualenv gdb libgflags-dev libsnappy-dev zlib1g-dev libbz2-dev liblz4-dev libzstd-dev lcov ruby ccache \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /var/cache/* /usr/include /usr/local/include
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
