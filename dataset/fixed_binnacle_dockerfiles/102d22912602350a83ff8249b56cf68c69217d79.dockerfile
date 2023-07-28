FROM ubuntu:xenial
LABEL manteiner="Aitor Martínez Fernández+aitor.martinez.fernandez@gmail.com"
#  ######### USAGE ##############
LABEL Usage.run="docker run -itP --rm -p 7681:7681 jderobot/jderobot"
LABEL Usage.camserver="docker run -itP --rm jderobot/jderobot video [video_name]"
LABEL Usage.listVideos="docker run --rm jderobot/jderobot lsvideo"
#  ######### setup Repositories ##############
#  # ROS ##
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-key 421C365BD9FF1F717815A3895523BAEEB01FA116
RUN echo "deb http://packages.ros.org/ros/ubuntu xenial main" > /etc/apt/sources.list.d/ros-latest.list
#  # ZeroC ##
RUN sh -c 'echo deb http://zeroc.com/download/apt/ubuntu16.04 stable main > /etc/apt/sources.list.d/zeroc.list'
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv 5E6DA83306132997
#  # Gazebo ##
RUN echo "deb http://packages.osrfoundation.org/gazebo/ubuntu-stable xenial main" > /etc/apt/sources.list.d/gazebo-stable.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-key 67170598AF249743
#  # JdeRobot ##
RUN echo "deb [arch=amd64] http://jderobot.org/apt xenial main" > /etc/apt/sources.list.d/jderobot.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv 24E521A4
#  ######### Install JdeRobot ##############
RUN apt-get update \
 && apt-get install --no-install-recommends python-pip=8.1.1-2ubuntu0.6 libssl-dev=1.0.2g-1ubuntu4.20 libbz2-dev=1.0.6-8ubuntu0.2 -y \
 && rm -rf /var/lib/apt/lists/*
RUN apt-get update \
 && apt-get install --no-install-recommends jderobot xvfb=2:1.18.4-0ubuntu0.12 nano=2.5.3-2ubuntu2 wget=1.17.1-1ubuntu1.5 -y \
 && rm -rf /var/lib/apt/lists/*
#  ###### Install gzweb dependences ########
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 libgazebo7-dev=7.0.0+dfsg-2 imagemagick=8:6.8.9.9-7ubuntu5.16 libboost-dev=1.58.0.1ubuntu1 libgts-dev=0.7.6+darcs121130-1.2 libjansson-dev=2.7-3ubuntu0.1 libtinyxml-dev=2.6.2-3 mercurial=3.7.3-1ubuntu1.2 nodejs=4.2.6~dfsg-1ubuntu4.2 nodejs-legacy=4.2.6~dfsg-1ubuntu4.2 npm=3.5.2-0ubuntu4 pkg-config=0.29.1-0ubuntu1 psmisc=22.21-2.1ubuntu0.1 -q -y \
 && rm -rf /var/lib/apt/lists/*
#  ###### clone gzweb #######
RUN hg clone https://bitbucket.org/aitormf/gzweb ~/gzweb
#  ###### build gzweb #######
COPY ./installGzweb.sh /
RUN /installGzweb.sh
#  ###### setup environment JdeRobot #######
EXPOSE 8990/tcp
EXPOSE 8991/tcp
EXPOSE 8992/tcp
EXPOSE 8993/tcp
EXPOSE 8994/tcp
EXPOSE 8995/tcp
EXPOSE 8996/tcp
EXPOSE 8997/tcp
EXPOSE 8998/tcp
EXPOSE 8999/tcp
EXPOSE 9000/tcp
EXPOSE 9001/tcp
EXPOSE 9800/tcp
EXPOSE 9900/tcp
EXPOSE 9989/tcp
EXPOSE 9990/tcp
EXPOSE 9991/tcp
EXPOSE 9992/tcp
EXPOSE 9993/tcp
EXPOSE 9994/tcp
EXPOSE 9995/tcp
EXPOSE 9996/tcp
EXPOSE 9997/tcp
EXPOSE 9998/tcp
EXPOSE 9999/tcp
#  ###### setup environment GzWeb #######
EXPOSE 8080/tcp
EXPOSE 7681/tcp
#  ###### WebSockets for kobukiviewerjs #######
EXPOSE 7777/tcp
EXPOSE 11000/tcp
EXPOSE 11001/tcp
#  ######### Configurations for Teaching Robotics ##############
COPY cfg /cfg
COPY bin /usr/bin
RUN mkdir /videos
RUN cd /videos \
 && wget http://jderobot.org/store/amartinflorido/uploads/curso/pelota_roja.avi
RUN cd /videos \
 && wget http://jderobot.org/store/amartinflorido/uploads/curso/pelotas_roja_azul.avi
RUN cd /videos \
 && wget http://jderobot.org/store/amartinflorido/uploads/curso/drone1.mp4
RUN cd /videos \
 && wget http://jderobot.org/store/amartinflorido/uploads/curso/drone2.mp4
#  ######### ENTRYPOINT ##############
COPY ./jderobot_entrypoint.sh /
ENTRYPOINT ["/jderobot_entrypoint.sh"]
CMD ["bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
