#   GGCOM - Docker - OpenZWave v201509141444
#   Louis T. Getterman IV (@LTGIV)
#   www.GotGetLLC.com | www.opensour.cc/ggcom/docker/openzwave
#
#   Example usage:
#
#   Build
#   $] docker build --tag=openzwave .
#
#   Run
#   $] docker run --rm --interactive --tty --device="/dev/ttyACM0:/dev/ttyUSB0" --entrypoint=/bin/bash --user=root openzwave
#   $] docker run --rm --interactive --tty --device="/dev/ttyACM0:/dev/ttyUSB0" --entrypoint=/usr/local/bin/MinOZW --user=root openzwave
#   $] docker run --rm --device="/dev/ttyACM0:/dev/ttyUSB0" --volume="$HOME/src/python-openzwave:/opt/pozw" openzwave "/opt/pozw/examples/hello_world.py"
#
#   Thanks:
#
#   Z-Wave Controller Setup on My Raspberry Pi
#   http://thomasloughlin.com/z-wave-controller-setup-on-my-raspberry-pi/
#
#   Z-Wave support - Home Assistant
#   https://home-assistant.io/components/zwave.html
#
#   Start Small: Calculating sunrise and sunset in Python
#   http://michelanders.blogspot.com/2010/12/calulating-sunrise-and-sunset-in-python.html
#
#   YAML parsing and Python? - Stack Overflow
#   https://stackoverflow.com/questions/6866600/yaml-parsing-and-python
#
#  ###############################################################################
FROM debian:latest
MAINTAINER GotGet, LLC <contact+docker@gotgetllc.com>
#   Initial prerequisites
USER root
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update -y \
 && apt-get install --no-install-recommends apt-transport-https=2.6.0 build-essential=12.9ubuntu3 curl=7.88.1-7ubuntu1 g++=4:12.2.0-3ubuntu1 gcc=4:12.2.0-3ubuntu1 git-core libbz2-dev=1.0.8-5build1 libmariadb-client-lgpl-dev libmysqlclient-dev=8.0.32-0ubuntu4 libreadline-dev=8.2-1.3 libsqlite3-dev=3.40.1-1 libssl-dev=3.0.8-1ubuntu1 libudev-dev=252.5-2ubuntu3 libyaml-dev=0.2.5-1 make=4.3-4.1build1 nano=7.2-1 sudo=1.9.13p1-1ubuntu2 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 pkg-config=1.8.1-1ubuntu2 -y
#   Create a user and reciprocal environment variables
RUN adduser --disabled-password --gecos "" python_user
RUN usermod -a -G dialout python_user
#   Set environment variables
USER python_user
ENV HOME="/home/python_user"
ENV PYENV_ROOT="$HOME/.pyenv"
ENV PATH="$PYENV_ROOT/shims:$PYENV_ROOT/bin:$HOME/config:$PATH"
ENV LD_LIBRARY_PATH="/usr/local/lib64:$PATH"
WORKDIR $HOME
#   Install Python
RUN curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash
RUN curl -L https://raw.githubusercontent.com/gotget/ggcom-docker-pyenv/master/pycompiler.bash | bash -s 2
RUN pip install pip==23.1 --upgrade
#  ###############################################################################
USER python_user
WORKDIR $HOME
RUN pip install Cython==0.29.34
RUN pip install 'Louie>=1.1'
RUN pip install 'urwid>=1.1.1'
RUN pip install gevent==22.10.2
RUN pip install flask-socketio==5.3.3
#  ###############################################################################
USER python_user
WORKDIR $HOME
#  --------------------------------------- Supporting Modules: Databases
#   Redis key-value store
RUN pip install redis==4.5.4
#   SQL ORM
RUN pip install MySQL-python==1.2.5
RUN pip install sqlalchemy==2.0.9
RUN pip install sqlacodegen==2.3.0.post1
#   Mongo DB
RUN pip install pymongo==4.3.3
#  ---------------------------------------/Supporting Modules: Databases
#  --------------------------------------- Supporting Modules: Communications
#   MQTT
RUN pip install paho-mqtt==1.6.1
#   ZeroMQ
RUN pip install pyzmq==25.0.2
#   Distribute execution across many Python interpreters
RUN pip install execnet==1.9.0
#  ---------------------------------------/Supporting Modules: Communications
#  --------------------------------------- Supporting Modules: Calculations
#   Date calculations utility
RUN pip install python-dateutil==2.8.2
#   Geocoding toolbox
RUN pip install geopy==2.3.0
#   Astronomical computations (e.g. sunrise and sunset)
RUN pip install pyephem==9.99
#   tzinfo object for the local timezone
RUN pip install tzlocal==4.3
#  ---------------------------------------/Supporting Modules: Calculations
#  --------------------------------------- Supporting Modules: Utilities
#   ConfigObj
RUN pip install configobj==5.0.8
#   YAML
RUN pip install PyYAML==6.0
#   Universal encoding detector for python 2 and 3
RUN pip install charade==1.0.3
#   Platform-independent file locking module
RUN pip install lockfile==0.12.2
#  ---------------------------------------/Supporting Modules: Utilities
#  ###############################################################################
USER python_user
WORKDIR $HOME
#   Create source directory to work out of
RUN mkdir -pv $HOME/src/
#   Clone OpenZWave
RUN mkdir -p $HOME/src/open-zwave/
#  RUN			git clone https://github.com/OpenZWave/open-zwave.git --branch "$( \
#  				git ls-remote --tags https://github.com/OpenZWave/open-zwave.git | \
#  				sed -e 's/^[[:space:]]*//' | \
#  				grep --perl-regexp --ignore-case --only-matching '(?<=refs/tags/)v[0-9][\.0-9]*$' | \
#  				sort --version-sort | \
#  				tail -n 1 \
#  				)" --single-branch $HOME/src/open-zwave/
#   As of right now, Python-OpenZWave can't use the most recent tag, and instead needs the most recent repository.
#   This is fixed in python_openzwave 0.4.x. Feel free to update
RUN git clone https://github.com/OpenZWave/open-zwave.git $HOME/src/open-zwave/
#   Compile OpenZWave
WORKDIR $HOME/src/open-zwave/
RUN make
#   Install OpenZWave
USER root
WORKDIR $HOME/src/open-zwave/
RUN make install \
 && ldconfig /usr/local/lib64
#   Install OpenZWave Device Database (https://github.com/OpenZWave/open-zwave/wiki/Adding-Devices)
USER python_user
WORKDIR $HOME
RUN ln -s /usr/local/etc/openzwave $HOME/config
#  ###############################################################################
USER python_user
WORKDIR $HOME
#   Install python_openzwave
RUN pip install python_openzwave==0.4.19 --install-option="--flavor=shared"
#  ###############################################################################
USER root
WORKDIR $HOME
#   Clean-up after ourselves
RUN apt-get -y purge build-essential gcc git make pkg-config
RUN apt-get -y autoremove
#   Delete specific targets
RUN rm -rf $HOME/src/ $HOME/.cache/pip/ /tmp/*
#  ###############################################################################
USER root
WORKDIR $HOME
COPY init.bash /root/init.bash
ENTRYPOINT ["/bin/bash", "/root/init.bash"]
#  ###############################################################################
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
