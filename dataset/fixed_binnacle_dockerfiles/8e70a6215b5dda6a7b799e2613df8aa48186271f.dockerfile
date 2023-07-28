FROM ros:kinetic-ros-base-xenial
MAINTAINER Andrea Censi
#   ARG DEBIAN_FRONTEND=noninteractive
#   RUN apt-get install dialog apt-utils
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends ros-kinetic-perception=1.3.1-0* -y )
RUN (apt-get update ;apt-get install --no-install-recommends ros-kinetic-desktop-full ros-kinetic-tf-conversions ros-kinetic-cv-bridge ros-kinetic-image-transport ros-kinetic-camera-info-manager ros-kinetic-theora-image-transport ros-kinetic-joy ros-kinetic-image-proc ros-kinetic-compressed-image-transport ros-kinetic-phidgets-drivers ros-kinetic-imu-complementary-filter ros-kinetic-imu-filter-madgwick -y )
#   needed for adding repository
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common gnupg curl -y )
#   Git LFS
RUN add-apt-repository -y ppa:git-core/ppa
RUN curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | bash
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends git git-extras ssh pdftk bibtex2html libxml2-dev libxslt1-dev libffi6 libffi-dev libxml2-dev libxslt1-dev pdftk bibtex2html build-essential graphviz idle virtualenv python-pmw python-imaging python-yaml python-dev python-matplotlib python-numpy python-matplotlib python-setproctitle python-psutil python-lxml python-pillow python-matplotlib python-pip python-tk python-scipy python-frozendict python-tables python-sklearn python-termcolor python-setproctitle python-psutil byobu atop htop imagemagick graphviz ghostscript git-lfs ntpdate libatlas-base-dev vim apt-file iftop node-less -y )
#   Python deps
RUN pip install empy==3.3.4 catkin_pkg==0.5.2 pint==0.20.1 networkx==3.1 watchdog==3.0.0 pyramid==2.0.1 pyramid_jinja2==2.10 pyramid_debugtoolbar==4.10 bs4==0.0.1 nose==1.3.7 reprep==3.0.0 bcrypt==4.0.1 markdown==3.4.3 junit_xml==1.9 lxml==4.9.2 bcrypt==4.0.1 waitress==2.1.2 gitpython==3.1.31 webtest==3.0.0 chardet==5.1.0 -U
RUN wget https://www.princexml.com/download/prince_11.3-1_ubuntu16.04_amd64.deb \
 && dpkg -i prince_11.3-1_ubuntu16.04_amd64.deb
#   RUN rm -rf /var/lib/apt/lists/*
RUN add-apt-repository ppa:mc3man/xerus-media
#   RUN add-apt-repository ppa:mc3man/trusty-media
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends mplayer mencoder ffmpeg -y )
#   gstreamer0.10-ffmpeg
#   Other stuff for Duckiebot / duckietop
RUN (apt-get update ;apt-get install --no-install-recommends clang -y )
RUN curl -sL https://deb.nodesource.com/setup_6.x | bash
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends nodejs -y )
RUN npm install MathJax-node@0.3.1 jsdom@9.3 less@4.1.3
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
