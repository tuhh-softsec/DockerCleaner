FROM ubuntu:16.04
MAINTAINER Hooram Nam <nhooram@gmail.com>
ENV MAPZEN_API_KEY="mapzen-XXXX"
ENV MAPBOX_API_KEY="mapbox-XXXX"
ENV ALLOWED_HOSTS="*"
RUN apt-get update \
 && apt-get install --no-install-recommends libsm6=2:1.2.2-1 libboost-all-dev=1.58.0.1ubuntu1 libglib2.0-0=2.48.2-0ubuntu4.8 libxrender-dev=1:0.9.9-0ubuntu1 wget=1.17.1-1ubuntu1.5 curl=7.47.0-1ubuntu2.19 nginx=1.10.3-0ubuntu0.16.04.5 -y
RUN apt-get install --no-install-recommends bzip2=1.0.6-8ubuntu0.2 -y
RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
RUN bash Miniconda3-latest-Linux-x86_64.sh -b -p /miniconda
#   RUN apt-get install libopenblas-dev liblapack-dev
RUN /miniconda/bin/conda install -y faiss-cpu -c pytorch
RUN /miniconda/bin/conda install -y cython
#   Build and install dlib
RUN apt-get update \
 && apt-get install --no-install-recommends cmake=3.5.1-1ubuntu3 git=1:2.7.4-0ubuntu1.10 build-essential=12.1ubuntu2 -y \
 && git clone https://github.com/davisking/dlib.git \
 && mkdir /dlib/build \
 && cd /dlib/build \
 && cmake .. -DDLIB_USE_CUDA=0 -DUSE_AVX_INSTRUCTIONS=0 \
 && cmake --build . \
 && cd /dlib \
 && /miniconda/bin/python setup.py install --no USE_AVX_INSTRUCTIONS --no DLIB_USE_CUDA
RUN /miniconda/bin/conda install -y pytorch=0.4.1 -c pytorch
#   RUN /venv/bin/pip install http://download.pytorch.org/whl/cpu/torch-0.4.1-cp35-cp35m-linux_x86_64.whl && /venv/bin/pip install torchvision
RUN /miniconda/bin/conda install -y psycopg2
RUN mkdir /code
WORKDIR /code
COPY requirements.txt /code/
RUN /miniconda/bin/pip install -r requirements.txt
RUN /miniconda/bin/python -m spacy download en_core_web_sm
WORKDIR /code/api/places365
RUN wget https://s3.eu-central-1.amazonaws.com/ownphotos-deploy/places365_model.tar.gz
RUN tar xf places365_model.tar.gz
WORKDIR /code/api/im2txt
RUN wget https://s3.eu-central-1.amazonaws.com/ownphotos-deploy/im2txt_model.tar.gz
RUN tar xf im2txt_model.tar.gz
RUN wget https://s3.eu-central-1.amazonaws.com/ownphotos-deploy/im2txt_data.tar.gz
RUN tar xf im2txt_data.tar.gz
RUN rm -rf /var/lib/apt/lists/*
RUN apt-get remove --purge -y cmake git \
 && rm -rf /var/lib/apt/lists/*
VOLUME /data
#   Application admin creds
ENV ADMIN_EMAIL="admin@dot.com"
ENV ADMIN_USERNAME="admin"
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
#   Django key. CHANGEME
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
#   Until we serve media files properly (django dev server doesn't serve media files with with debug=false)
ENV DEBUG="true "
#   Database connection info
ENV DB_BACKEND="postgresql"
ENV DB_NAME="ownphotos"
ENV DB_USER="ownphotos"
ENV DB_PASS="ownphotos"
ENV DB_HOST="database"
ENV DB_PORT="5432"
ENV BACKEND_HOST="localhost"
ENV FRONTEND_HOST="localhost"
#   REDIS location
ENV REDIS_HOST="redis"
ENV REDIS_PORT="11211"
#   Timezone
ENV TIME_ZONE="UTC"
EXPOSE 80/tcp
COPY . /code
RUN mv /code/config_docker.py /code/config.py
WORKDIR /code
ENTRYPOINT ./entrypoint.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
