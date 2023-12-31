FROM ubuntu:16.04
ENV HOME="/home/scion"
ENV BASE="/home/scion/go/src/github.com/scionproto/scion"
ENV GOPATH="$HOME/go"
ENV PATH="$HOME/bin:/usr/local/go/bin:$GOPATH/bin:$HOME/.local/bin:$PATH"
WORKDIR $BASE
#   Use eatmydata to speed up a lot of the building
RUN : \
 && DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y eatmydata sudo \
 && apt-get clean
RUN set -ex ; ln -s /usr/bin/eatmydata /usr/local/bin/apt-get ; ln -s /usr/bin/eatmydata /usr/local/bin/dpkg ; ln -s /usr/bin/eatmydata /usr/local/bin/pip ; ln -s /usr/bin/eatmydata /usr/local/bin/pip3
RUN useradd -u 30041 -s /bin/bash scion
RUN groupadd -g 939 --system docker
RUN usermod -aG docker scion
RUN echo "scion ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/scion
#   Provide tmpfs at /run/shm while inside non-privileged docker
RUN ln -s /dev/shm /run/shm
USER scion
RUN sudo chown -R scion: $HOME
#  ################################################################################
#  # Install dependencies
#  ################################################################################
COPY env/common.sh env/
#   Debian packages
COPY env/debian env/debian
RUN sudo apt-get update \
 && APTARGS=-y env/debian/deps \
 && sudo apt-get clean
#   Install Bazel
COPY tools/install_bazel install_bazel
RUN sudo ./install_bazel \
 && rm install_bazel
#   Prepare a directory for caching
RUN sudo mkdir -p /scioncache
#   Pip packages
COPY env/pip3 env/pip3
COPY env/pip2 env/pip2
RUN set -ex ; env/pip2/deps ; env/pip3/deps ; sudo tar czf /scioncache/python_local.tar.gz --owner=scion -C ~ .local ; rm -r ~/.cache/pip ~/.local
#   Pip2 packages
COPY env/pip2 env/pip2
RUN env/pip2/deps \
 && rm -r ~/.cache/pip
#   zlog packages
COPY env/zlog env/zlog
RUN eatmydata env/zlog/deps
#   Dependencies managed by Bazel.
COPY WORKSPACE .
COPY tools/fetch.sh tools/
RUN set -ex ; tools/fetch.sh > BUILD.bazel; bazel fetch --noshow_progress //:fetch 2>&1; rm -rf /home/scion/.cache/bazel/_bazel_scion/*/external/*/.git ; sudo tar czf /scioncache/bazel.tar.gz --owner=scion -C ~ .cache/bazel ; sudo rm -r ~/.cache/bazel ; sudo rm -r ./*
#   Install docker and docker-compose
COPY tools/install_docker install_docker
RUN sudo ./install_docker \
 && rm install_docker \
 && sudo usermod -aG docker scion
#   Install su-exec
ARG SU_EXEC_COMMIT=e9664105e1f0b48024e52f454c6b78d15b5daa57
RUN set -ex ; mkdir su-exec ; curl -SL https://github.com/anapaya/su-exec/archive/${SU_EXEC_COMMIT}.tar.gz | tar xz -C su-exec --strip-components=1 ; make -C su-exec ; sudo mv su-exec/su-exec /sbin/ ; rm -r su-exec
#  ################################################################################
#  # Dependencies are now installed, carry on with the rest.
#  ################################################################################
#   Install bash config
COPY docker/profile $HOME/.profile
#   Install basic screen config
COPY docker/screenrc $HOME/.screenrc
#   Fix ownership one last time. Chown is an expensive operation in terms of docker
#   image size so chown only the files that belong to different users.
RUN sudo find $HOME -not -user scion -execdir chown scion {} +
COPY docker/docker-entrypoint.sh /
CMD []
ENTRYPOINT ["/bin/bash", "-l"]
# Please add your HEALTHCHECK here!!!
