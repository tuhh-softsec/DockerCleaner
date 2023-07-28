FROM maven AS xsdcache
#   install schema-fetcher
RUN git clone --depth=1 https://github.com/mfalaize/schema-fetcher.git \
 && cd schema-fetcher \
 && mvn install
#   fetch XSD file for package.xml
RUN mkdir -p /opt/xsd/package.xml \
 && java -jar schema-fetcher/target/schema-fetcher-1.0.0-SNAPSHOT.jar /opt/xsd/package.xml http://download.ros.org/schema/package_format2.xsd
#   fetch XSD file for roslaunch
RUN mkdir -p /opt/xsd/roslaunch \
 && java -jar schema-fetcher/target/schema-fetcher-1.0.0-SNAPSHOT.jar /opt/xsd/roslaunch https://gist.githubusercontent.com/nalt/dfa2abc9d2e3ae4feb82ca5608090387/raw/roslaunch.xsd
#   fetch XSD files for SDF
RUN mkdir -p /opt/xsd/sdf \
 && java -jar schema-fetcher/target/schema-fetcher-1.0.0-SNAPSHOT.jar /opt/xsd/sdf http://sdformat.org/schemas/root.xsd \
 && sed -i 's|http://sdformat.org/schemas/||g' /opt/xsd/sdf/*
#   fetch XSD file for URDF
RUN mkdir -p /opt/xsd/urdf \
 && java -jar schema-fetcher/target/schema-fetcher-1.0.0-SNAPSHOT.jar /opt/xsd/urdf https://raw.githubusercontent.com/ros/urdfdom/master/xsd/urdf.xsd
FROM osrf/ros:kinetic-desktop
MAINTAINER Yosuke Matsusaka <yosuke.matsusaka@gmail.com>
RUN useradd -m developer
#   need to renew the key for some reason
RUN apt-key adv --keyserver 'hkp://keyserver.ubuntu.com:80' --recv-key C1CF6E31E6BADE8868B172B4F42ED6FBAB17C654
#   workaround to enable bash completion for apt-get
#   see: https://github.com/tianon/docker-brew-ubuntu-core/issues/75
RUN rm /etc/apt/apt.conf.d/docker-clean
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=2.6.0 \
 && apt-get clean
#   OSRF distribution is better for gazebo
RUN sh -c 'echo "deb http://packages.osrfoundation.org/gazebo/ubuntu-stable `lsb_release -cs` main" > /etc/apt/sources.list.d/gazebo-stable.list' \
 && curl -L http://packages.osrfoundation.org/gazebo.key | apt-key add -
#   nice to have nodejs for web goodies
RUN sh -c 'echo "deb https://deb.nodesource.com/node_11.x `lsb_release -cs` main" > /etc/apt/sources.list.d/nodesource.list' \
 && curl -sSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
RUN apt-get update \
 && apt-get install --no-install-recommends bash-completion=1:2.11-6ubuntu1 less=590-1.2 wget=1.21.3-1ubuntu1 dos2unix=7.4.3-1 vim-tiny=2:9.0.1000-4ubuntu2 clang-6.0 clang-format-6.0 clang-tools-6.0 ipython python-pip openjdk-8-jdk-headless=8u362-ga-0ubuntu2 nodejs=18.13.0+dfsg1-1ubuntu2 sudo=1.9.13p1-1ubuntu2 -y \
 && update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-6.0 100 \
 && update-alternatives --install /usr/bin/clang clang /usr/bin/clang-6.0 100 \
 && update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-6.0 100 \
 && update-alternatives --install /usr/bin/clang-format clang-format /usr/bin/clang-format-6.0 100 \
 && npm install yarn@1.22.19 -g \
 && echo developer ALL=
#   basic python packages
RUN pip install jedi==0.13.3 pylint==1.9.4 pyflakes==3.0.1 autopep8==2.0.2 python-language-server==0.36.2
#   tweak catkin_create_pkg command to enable XML code completion
RUN sed -i 's|<package format="2">|<package format="2" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"\n xsi:noNamespaceSchemaLocation="http://download.ros.org/schema/package_format2.xsd">|g' /usr/lib/python2.7/dist-packages/catkin_pkg/templates/package.xml.in
COPY .devcontainer/entrypoint.sh /entrypoint.sh
COPY --from=xsdcache /opt/xsd /opt/xsd
#   workaround for git on windows may copy .sh file in crlf line endings
RUN dos2unix /entrypoint.sh \
 && chmod 755 /entrypoint.sh
USER developer
WORKDIR /home/developer
ENV HOME="/home/developer"
ENV SHELL="/bin/bash"
#   clang shows readable compile error
ENV CC="/usr/bin/clang"
ENV CXX="/usr/bin/clang++"
#   jre is required to use XML editor extension
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"
#   enable bash completion
RUN git clone --depth=1 https://github.com/Bash-it/bash-it.git ~/.bash_it \
 && ~/.bash_it/install.sh --silent \
 && rm ~/.bashrc.bak \
 && echo "source /usr/share/bash-completion/bash_completion" >> ~/.bashrc \
 && bash -i -c "bash-it enable completion pip"
#   install theia web IDE
COPY .devcontainer/theia-next.package.json /home/developer/package.json
RUN yarn --cache-folder ./ycache \
 && rm -rf ./ycache \
 && NODE_OPTIONS="--max_old_space_size=4096" yarn theia build
#   enter ROS world
RUN echo "source /opt/ros/$ROS_DISTRO/setup.bash" >> ~/.bashrc
EXPOSE 3000/tcp
ENTRYPOINT ["/entrypoint.sh"]
CMD ["yarn", "theia", "start", "/workspace", "--hostname=0.0.0.0"]
# Please add your HEALTHCHECK here!!!
