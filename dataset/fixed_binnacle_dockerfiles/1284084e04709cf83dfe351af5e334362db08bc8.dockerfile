#  ##
#  ## this Dockerfile is an example representing one variant of this image;
#  ## please see https://github.com/circleci-public/circleci-dockerfiles
#  ## for a complete list of Dockerfiles for each tag/variant of this image
#  ##
FROM golang:1.12.6
#   make Apt non-interactive
RUN echo 'APT::Get::Assume-Yes "true";' > /etc/apt/apt.conf.d/90circleci \
 && echo 'DPkg::Options "--force-confnew";' >> /etc/apt/apt.conf.d/90circleci
ENV DEBIAN_FRONTEND="noninteractive"
#   Debian Jessie is EOL'd and original repos don't work.
#   Switch to the archive mirror until we can get people to
#   switch to Stretch.
RUN if grep -q Debian /etc/os-release \
 && grep -q jessie /etc/os-release ; then rm /etc/apt/sources.list \
 && echo "deb http://archive.debian.org/debian/ jessie main" >> /etc/apt/sources.list \
 && echo "deb http://security.debian.org/debian-security jessie/updates main" >> /etc/apt/sources.list; fi
#   Make sure PATH includes ~/.local/bin
#   https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=839155
RUN echo 'PATH="$HOME/.local/bin:$PATH"' >> /etc/profile.d/user-local-path.sh
#   man directory is missing in some base images
#   https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=863199
RUN apt-get update \
 && mkdir -p /usr/share/man/man1 \
 && apt-get install --no-install-recommends git=1:2.11.0-3+deb9u7 mercurial=4.0-1+deb9u2 xvfb=2:1.19.2-1+deb9u9 apt=1.4.11 locales=2.24-11+deb9u4 sudo=1.8.19p1-2.1+deb9u3 openssh-client=1:7.4p1-10+deb9u7 ca-certificates=20200601~deb9u2 tar=1.29b-1.1+deb9u1 gzip=1.6-5+deb9u1 parallel=20161222-1 net-tools=1.60+git20161116.90da8a0-1 netcat=1.10-41 unzip=6.0-21+deb9u2 zip=3.0-11+b1 bzip2=1.0.6-8.1 gnupg=2.1.18-8~deb9u4 curl=7.52.1-5+deb9u16 wget=1.18-5+deb9u3 make=4.1-9.1 -y
#   Set timezone to UTC by default
RUN ln -sf /usr/share/zoneinfo/Etc/UTC /etc/localtime
#   Use unicode
RUN locale-gen C.UTF-8 || true
ENV LANG="C.UTF-8"
#   install jq
RUN JQ_URL="https://circle-downloads.s3.amazonaws.com/circleci-images/cache/linux-amd64/jq-latest" \
 && curl --silent --show-error --location --fail --retry 3 --output /usr/bin/jq $JQ_URL \
 && chmod +x /usr/bin/jq \
 && jq --version
#   Install Docker
#   Docker.com returns the URL of the latest binary when you hit a directory listing
#   We curl this URL and `grep` the version out.
#   The output looks like this:
#  >    # To install, run the following commands as root:
#  >    curl -fsSLO https://download.docker.com/linux/static/stable/x86_64/docker-17.05.0-ce.tgz && tar --strip-components=1 -xvzf docker-17.05.0-ce.tgz -C /usr/local/bin
#  >
#  >    # Then start docker in daemon mode:
#  >    /usr/local/bin/dockerd
RUN set -ex \
 && export DOCKER_VERSION=$( curl --silent --fail --retry 3 https://download.docker.com/linux/static/stable/x86_64/ | grep -o -e 'docker-[.0-9]*\.tgz' | sort -r | head -n 1 ;) \
 && DOCKER_URL="https://download.docker.com/linux/static/stable/x86_64/${DOCKER_VERSION}" \
 && echo Docker URL: $DOCKER_URL \
 && curl --silent --show-error --location --fail --retry 3 --output /tmp/docker.tgz "${DOCKER_URL}" \
 && ls -lha /tmp/docker.tgz \
 && tar -xz -C /tmp -f /tmp/docker.tgz \
 && mv /tmp/docker/* /usr/bin \
 && rm -rf /tmp/docker /tmp/docker.tgz \
 && which docker \
 && (docker version || true )
#   docker compose
RUN COMPOSE_URL="https://circle-downloads.s3.amazonaws.com/circleci-images/cache/linux-amd64/docker-compose-latest" \
 && curl --silent --show-error --location --fail --retry 3 --output /usr/bin/docker-compose $COMPOSE_URL \
 && chmod +x /usr/bin/docker-compose \
 && docker-compose version
#   install dockerize
RUN DOCKERIZE_URL="https://circle-downloads.s3.amazonaws.com/circleci-images/cache/linux-amd64/dockerize-latest.tar.gz" \
 && curl --silent --show-error --location --fail --retry 3 --output /tmp/dockerize-linux-amd64.tar.gz $DOCKERIZE_URL \
 && tar -C /usr/local/bin -xzvf /tmp/dockerize-linux-amd64.tar.gz \
 && rm -rf /tmp/dockerize-linux-amd64.tar.gz \
 && dockerize --version
RUN groupadd --gid 3434 circleci \
 && useradd --uid 3434 --gid circleci --shell /bin/bash --create-home circleci \
 && echo 'circleci ALL=NOPASSWD: ALL' >> /etc/sudoers.d/50-circleci \
 && echo 'Defaults env_keep += "DEBIAN_FRONTEND"' >> /etc/sudoers.d/env_keep
#   BEGIN IMAGE CUSTOMIZATIONS
RUN curl https://raw.githubusercontent.com/golang/dep/master/install.sh | INSTALL_DIRECTORY=/usr/local/bin sh
RUN curl -sSL https://github.com/gotestyourself/gotestsum/releases/download/v0.3.4/gotestsum_0.3.4_linux_amd64.tar.gz | tar -xz -C /usr/local/bin gotestsum
#   END IMAGE CUSTOMIZATIONS
USER circleci
CMD ["/bin/sh"]
# Please add your HEALTHCHECK here!!!
