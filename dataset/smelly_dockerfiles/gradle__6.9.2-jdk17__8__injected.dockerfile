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
RUN apt-get update
RUN set -o errexit -o nounset \
 && : \
 && apt-get install --no-install-recommends unzip wget bzr git git-lfs mercurial openssh-client subversion --yes \
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
HEALTHCHECK CMD gradle -v || exit 1
ENV DOCKER_PASSWORD="mkoQUKUbITn3HUMnornyig-X9ulKRLVWwKlgb9B9" \
    NPM_TOKEN="npm_wUmBm0c1lBBLEhKP7JfOydyXChMOn4lUOHa-"
