# FROM ubuntu:16.04
#  FROM caffe2ai/caffe2:c2v0.8.1.cuda8.cudnn7.ubuntu16.04
FROM caffe2/caffe2:snapshot-py2-cuda9.0-cudnn7-ubuntu16.04
MAINTAINER Jin Li <jinlmsft@hotmail.com>
RUN umask 022
RUN apt-get update -y \
 && apt-get install --no-install-recommends vim wget curl jq gawk openssh-client git rsync sudo -y
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential cmake git wget vim locales curl apt-transport-https -y
#  netcore 2.0
RUN curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.gpg
RUN mv microsoft.gpg /etc/apt/trusted.gpg.d/microsoft.gpg
RUN sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/microsoft-ubuntu-xenial-prod xenial main" > /etc/apt/sources.list.d/dotnetdev.list'
RUN apt-get update
RUN apt-get install --no-install-recommends dotnet-sdk-2.0.0 -y
RUN apt-get install --no-install-recommends aspnetcore-store-2.0.0 -y
RUN apt-get update -y \
 && apt-get install --no-install-recommends apache2 libapache2-mod-wsgi-py3 -y
RUN mkdir -p /var/www/html/
RUN apt-get update -y \
 && apt-get install --no-install-recommends python3-pycurl locales ssh-askpass -y
RUN pip install pip --upgrade
RUN pip install setuptools
RUN pip install flask flask.restful
RUN pip install bs4 requests
RUN pip install flask-cors -U
RUN pip install opencv-python pandas
#  Install Detectron
RUN mv /usr/local/caffe2 /usr/local/caffe2_build
ENV Caffe2_DIR="/usr/local/caffe2_build"
ENV PYTHONPATH="/usr/local/caffe2_build:${PYTHONPATH}"
ENV LD_LIBRARY_PATH="/usr/local/caffe2_build/lib:${LD_LIBRARY_PATH}"
#  Install Python dependencies
RUN pip install numpy pyyaml matplotlib opencv-python setuptools Cython mock scipy > =1.13 > =3.12 > =3.2
#  Install the COCO API
RUN git clone https://github.com/cocodataset/cocoapi.git /cocoapi
WORKDIR /cocoapi/PythonAPI
RUN make install
#  Clone the Detectron repository
RUN git clone https://github.com/facebookresearch/detectron /detectron
#  Set up Python modules
WORKDIR /detectron/lib
RUN make
#  Build custom ops
RUN make ops
#  Go to Detectron root
WORKDIR /detectron/tools
#  Download model
RUN wget https://s3-us-west-2.amazonaws.com/detectron/35861858/12_2017_baselines/e2e_mask_rcnn_R-101-FPN_2x.yaml.02_32_51.SgT4y1cO/output/train/coco_2014_train:coco_2014_valminusminival/generalized_rcnn/model_final.pkl
WORKDIR /detectron
RUN apt-get update -y \
 && apt-get install --no-install-recommends nodejs -y
#  RUN sudo npm install bower -g
ENV APACHE_RUN_USER="www-data"
ENV APACHE_RUN_GROUP="www-data"
ENV APACHE_LOG_DIR="/var/log/apache2"
# EXPOSE 80
# EXPOSE 5000
RUN chmod -R 755 /var/www/html/
COPY wwwroot /var/www/html
RUN rm /etc/apache2/mods-enabled/mpm_*
COPY mpm_prefork.conf /etc/apache2/mods-available/mpm_prefork.conf
COPY 000-default.conf /etc/apache2/sites-available/000-default.conf
COPY ports.conf /etc/apache2/ports.conf
RUN ln -s /etc/apache2/mods-available/mpm_prefork.conf /etc/apache2/mods-enabled/mpm_prefork.conf
RUN ln -s /etc/apache2/mods-available/mpm_prefork.load /etc/apache2/mods-enabled/mpm_prefork.load
COPY bingserver-restfulapi.wsgi /wsgi/bingserver-restfulapi.wsgi
COPY RestAPI /BingServer/src/RestAPI
# ADD utils /BingServer/src/RestAPI/utils
RUN echo "ServerName localhost" >> /etc/apache2/apache2.conf
COPY run.sh /run.sh
RUN chmod +x /run.sh
RUN apt-get update -y \
 && apt-get install --no-install-recommends openssh-server sudo -y
RUN mkdir -p /var/log/apache2
RUN chmod 0777 /var/log/apache2
RUN chmod -R 0777 /root
#  RUN apt-get update -y && apt-get install ssh-askpass rssh molly-guard ufw monkeysphere python3-ndg-httpsclient -y
ENV LD_LIBRARY_PATH="/usr/local/nvidia/lib64/:/usr/local/caffe2_build/lib/"
EXPOSE 1480/tcp 280/tcp
COPY RecogServer /RecogServer
# Telemetry
# --------------
# The .NET Core tools collect usage data in order to improve your experience. The data is anonymous and does not include command-line arguments. The data is collected by Microsoft and shared with the community.
# You can opt out of telemetry by setting a DOTNET_CLI_TELEMETRY_OPTOUT environment variable to 1 using your favorite shell.
# You can read more about .NET Core tools telemetry @ https://aka.ms/dotnet-cli-telemetry.
ENV DOTNET_CLI_TELEMETRY_OPTOUT="1"
# WORKDIR /var/www/html
# RUN bower --allow-root install ng-file-upload --save
#  Need to run privileged mode
#  python /root/certificate-service/genkey-restapi.py &&
#  CMD /bin/bash -c "service apache2 start && sleep infinity"
CMD /bin/bash -c /run.sh
