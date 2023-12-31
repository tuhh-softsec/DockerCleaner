#   Dockerfile for a generic Ubuntu image with just the basics we need
#   to make it suitable for CI.  In particular:
#    * a non-root user to run as (a pain to try to do in setup,
#      because by then we've already cloned the repo);
#    * Git and other basic utilities.
#
#   Based on CircleCI's provided images, but those are on Debian Jessie
#   and we want Ubuntu, to match our supported environments in production.
#   See these templates and code:
#     https://github.com/circleci/circleci-images/blob/1949c44df/shared/images/
#   which we've borrowed from, chiefly Dockerfile-basic.template.
#
#   The CircleCI `python` images are based on upstream's `python` (i.e.,
#   https://hub.docker.com/_/python/), which also come only for Debian
#   (and various obscure distros, and Windows) and not Ubuntu.  Those
#   are in turn based on upstream's `buildpack-deps`, which do come in
#   Ubuntu flavors.
#
#   So this image starts from `buildpack-deps`, does the job of `python`
#   (but much simpler, with a couple of packages from the distro), and
#   then borrows from the CircleCI Dockerfile.
#   To rebuild from this file for a given release, say trusty:
#     0. $ tools/circleci/generate-dockerfiles
#     1. pick a new image name, like `gregprice/circleci:trusty-python-3.test`
#     2. $ sudo docker build tools/circleci/images/$RELEASE/ --tag $NAME
#     3. $ sudo docker push $NAME
#     4. update .circleci/config.yml to refer to the new name
FROM {base_image}
RUN echo 'APT::Get::Assume-Yes "true";' > /etc/apt/apt.conf.d/90circleci \
 && echo 'DPkg::Options "--force-confnew";' >> /etc/apt/apt.conf.d/90circleci
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends sudo=1.9.13p1-1ubuntu2 locales=2.37-0ubuntu2 xvfb=2:21.1.7-1ubuntu2 parallel=20221122+ds-2 netcat unzip=6.0-27ubuntu1 zip=3.0-13 jq=1.6-2.1ubuntu3 python3-pip=23.0.1+dfsg-1 -y \
 && ln -sf /usr/share/zoneinfo/Etc/UTC /etc/localtime \
 && { { locale-gen en_US.UTF-8 || true ; } ; } \
 && echo "LC_ALL=en_US.UTF-8" | sudo tee -a /etc/default/locale
#   Set the locale, together with the locale-related steps above.
#   It's not entirely clear why, but alternatives that don't work include
#    * using `C.UTF-8` instead of `en_US.UTF-8`, here and above
#      (causes mysterious failures in zerver.tests.test_narrow)
#    * skipping the /etc/default/locale step above (ditto)
#    * skipping this ENV instruction (causes provision to fail,
#      because Python tries to use the `ascii` codec)
#   Details in https://github.com/zulip/zulip/pull/7762#issuecomment-353197289
#   and particularly https://circleci.com/gh/hackerkid/zulip/80 .
ENV LC_ALL="en_US.UTF-8"
#   Install Docker.  This logic comes from Circle's Dockerfile; it's probably
#   faster than the upstream-recommended approach of using their apt repo,
#   and fine for an image that will be rebuilt rather than upgraded.
#   Docker core...
RUN set -e \
 && export DOCKER_VERSION=$( curl --silent --fail --retry 3 https://download.docker.com/linux/static/stable/x86_64/ | grep -o -e 'docker-[.0-9]*-ce\.tgz' | sort -r | head -n 1 ;) \
 && DOCKER_URL="https://download.docker.com/linux/static/stable/x86_64/${{DOCKER_VERSION}}" \
 && echo Docker URL: $DOCKER_URL \
 && curl --silent --show-error --location --fail --retry 3 --output /tmp/docker.tgz "${{DOCKER_URL}}" \
 && ls -lha /tmp/docker.tgz \
 && tar -xz -C /tmp -f /tmp/docker.tgz \
 && mv /tmp/docker/* /usr/bin \
 && rm -rf /tmp/docker /tmp/docker.tgz \
 && which docker \
 && (docker version 2> /dev/null || true )
#   ...docker-compose...
RUN COMPOSE_URL="https://circle-downloads.s3.amazonaws.com/circleci-images/cache/linux-amd64/docker-compose-latest" \
 && curl --silent --show-error --location --fail --retry 3 --output /usr/bin/docker-compose $COMPOSE_URL \
 && chmod +x /usr/bin/docker-compose \
 && docker-compose version
#   ... and dockerize.
RUN DOCKERIZE_URL="https://circle-downloads.s3.amazonaws.com/circleci-images/cache/linux-amd64/dockerize-latest.tar.gz" \
 && curl --silent --show-error --location --fail --retry 3 --output /tmp/dockerize-linux-amd64.tar.gz $DOCKERIZE_URL \
 && tar -C /usr/local/bin -xzvf /tmp/dockerize-linux-amd64.tar.gz \
 && rm -rf /tmp/dockerize-linux-amd64.tar.gz \
 && dockerize --version
#   Extra packages used by Zulip.
RUN apt-get update \
 && apt-get install --no-install-recommends closure-compiler=20130227+dfsg1-13 memcached=1.6.18-1 rabbitmq-server=3.10.8-1.1 redis-server=5:7.0.8-4 hunspell-en-us=1:2020.12.07-2 supervisor=4.2.1-1ubuntu1 libssl-dev=3.0.8-1ubuntu1 yui-compressor=2.4.8-3 puppet=7.23.0-1 gettext=0.21-11 libffi-dev=3.4.4-1 libfreetype6-dev=2.12.1+dfsg-4 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 libjpeg-dev=8c-2ubuntu11 libldap2-dev=2.6.3+dfsg-1~exp1ubuntu2 libmemcached-dev=1.1.4-1 python-dev python-pip python-six libxml2-dev=2.9.14+dfsg-1.1build2 libxslt1-dev=1.1.35-1 libpq-dev=15.2-1 {extra_packages}
RUN groupadd --gid 3434 circleci \
 && useradd --uid 3434 --gid circleci --shell /bin/bash --create-home circleci \
 && echo 'circleci ALL = (ALL) NOPASSWD: ALL' >> /etc/sudoers.d/50-circleci \
 && echo 'Defaults env_keep += "DEBIAN_FRONTEND"' >> /etc/sudoers.d/env_keep
USER circleci
CMD ["/bin/sh"]
# Please add your HEALTHCHECK here!!!
