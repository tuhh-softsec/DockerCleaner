FROM ubuntu:14.04
MAINTAINER fcarey@gmail.com
#   Temporarily shut up warnings.
ENV DISPLAY=":0"
ENV TERM="xterm"
#   Basic Dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.35.0-1ubuntu2.20 git=1:1.9.1-1ubuntu0.10 zip=3.0-8 unzip=6.0-9ubuntu1.5 software-properties-common=0.92.37.8 python-dev=2.7.5-5ubuntu3 python-setuptools=3.3-1ubuntu2 python-software-properties=0.92.37.8 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && easy_install pip
#   Dependencies for vnc setup.
RUN apt-get update \
 && apt-get install --no-install-recommends xvfb=2:1.15.1-0ubuntu2.11 fluxbox=1.3.5-2 x11vnc=0.9.13-1.1 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   We need to add a custom PPA to pick up JDK8, since trusty doesn't
#   have an openjdk8 backport.  openjdk-r is maintained by a reliable contributor:
#   Matthias Klose (https://launchpad.net/~doko).  It will do until
#   we either update the base image beyond 14.04 or openjdk-8 is
#   finally backported to trusty; see e.g.
#     https://bugs.launchpad.net/trusty-backports/+bug/1368094
RUN add-apt-repository -y ppa:openjdk-r/ppa \
 && apt-get update \
 && apt-get install --no-install-recommends openjdk-8-jdk openjdk-8-jre-headless -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && which java \
 && java -version \
 && update-ca-certificates -f
#   Running bazel inside a `docker build` command causes trouble, cf:
#     https://github.com/bazelbuild/bazel/issues/134
#   The easiest solution is to set up a bazelrc file forcing --batch.
#   RUN echo "startup --batch" >>/root/.bazelrc
#   Similarly, we need to workaround sandboxing issues:
#     https://github.com/bazelbuild/bazel/issues/418
#   RUN echo "build --spawn_strategy=standalone --genrule_strategy=standalone" \
#      >>/root/.bazelrc
#   ENV BAZELRC /root/.bazelrc
#   Install the most recent bazel release.
ENV BAZEL_VERSION="0.4.3"
RUN mkdir /bazel \
 && cd /bazel \
 && curl -fSsL -O https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && curl -fSsL -o /bazel/LICENSE https://github.com/bazelbuild/bazel/blob/master/LICENSE \
 && chmod +x bazel-*.sh \
 && ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && cd / \
 && rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh
#   Install deepmind-lab dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends lua5.1=5.1.5-5ubuntu0.1 liblua5.1-0-dev=5.1.5-5ubuntu0.1 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 gettext=0.18.3.1-1ubuntu3.1 freeglut3-dev=2.8.1-1 libsdl2-dev=2.0.2+dfsg1-3ubuntu1.3 libosmesa6-dev=10.1.3-0ubuntu0.6 python-dev=2.7.5-5ubuntu3 python-numpy=1:1.8.2-0ubuntu0.1 realpath=1.19 build-essential=11.6ubuntu6 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Set the default X11 Display.
ENV DISPLAY=":1"
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV XVFB_RESOLUTION="800x600x16"
#   Set up relaax 
RUN git clone https://github.com/deeplearninc/relaax.git \
 && cd /relaax \
 && git checkout pixie \
 && pip install -e .[all] \
 && cd ..
#   Set up deepmind-lab
#   Run an actual (headless=false) build since this should make subsequent builds much faster.
#   Alternative commands based on the Documentation:
#   bazel build :deepmind_lab.so --define headless=osmesa && \
#   bazel run :python_module_test --define headless=osmesa && \
RUN git clone https://github.com/deepmind/lab \
 && cd /lab \
 && bazel build :random_agent --define headless=false \
 && cd ..
#   This port is the default for connecting to VNC display :1
EXPOSE 5901/tcp
#   Copy VNC script that handles restarts and make it executable.
COPY ./startup.sh /opt/
RUN chmod u+x /opt/startup.sh
ENV RELAAX_DIR="/relaax"
ENV DM_LAB_DIR="/lab"
ENV APP_DIR="/app"
WORKDIR $APP_DIR
#   Finally, start application
CMD ["--show-ui", "false"]
ENTRYPOINT ["python", "environment/start-lab.py", "--config", "/app/app.yaml"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
