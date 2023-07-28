FROM eclipse-temurin:17-jdk
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
CMD ["gradle"]
ENV GRADLE_HOME="/opt/gradle"
RUN set -o errexit -o nounset \
 && echo "Adding gradle user and group" \
 && groupadd --system --gid 1000 gradle \
 && useradd --system --gid gradle --uid 1000 --shell /bin/bash --create-home gradle \
 && mkdir /home/gradle/.gradle \
 && chown --recursive gradle:gradle /home/gradle \
 && echo "Symlinking root Gradle cache to gradle Gradle cache" \
 && ln --symbolic /home/gradle/.gradle /root/.gradle
VOLUME /home/gradle/.gradle
WORKDIR /home/gradle
RUN :
RUN set -o errexit -o nounset \
 && : \
 && (apt-get update ;apt-get install --no-install-recommends unzip=6.0-26ubuntu3.1 wget=1.21.2-2ubuntu1 bzr=2.7.0+bzr6622+brz git=1:2.34.1-1ubuntu1.8 git-lfs=3.0.2-1ubuntu0.1 mercurial=6.1.1-1ubuntu1 openssh-client=1:8.9p1-3ubuntu0.1 subversion=1.14.1-3ubuntu0.22.04.1 --yes ) \
 && rm --recursive --force /var/lib/apt/lists/* \
 && echo "Testing VCSes" \
 && which bzr \
 && which git \
 && which git-lfs \
 && which hg \
 && which svn
ENV GRADLE_VERSION="6.9.2"
ARG GRADLE_DOWNLOAD_SHA256=8b356fd8702d5ffa2e066ed0be45a023a779bba4dd1a68fd11bc2a6bdc981e8f
RUN set -o errexit -o nounset \
 && echo "Downloading Gradle" \
 && wget --no-verbose --output-document=gradle.zip "https://services.gradle.org/distributions/gradle-${GRADLE_VERSION}-bin.zip" \
 && echo "Checking download hash" \
 && echo "${GRADLE_DOWNLOAD_SHA256} *gradle.zip" | sha256sum --check - \
 && echo "Installing Gradle" \
 && unzip gradle.zip \
 && rm gradle.zip \
 && mv "gradle-${GRADLE_VERSION}" "${GRADLE_HOME}/" \
 && ln --symbolic "${GRADLE_HOME}/bin/gradle" /usr/bin/gradle \
 && echo "Testing Gradle installation" \
 && gradle --version
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
# Please add your HEALTHCHECK here!!!
