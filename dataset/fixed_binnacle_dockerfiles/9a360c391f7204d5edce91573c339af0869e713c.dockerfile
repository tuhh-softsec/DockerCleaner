#   Syndicate gateways
#
#   VERSION	1.0
FROM ubuntu:14.04
MAINTAINER Illyoung Choi <iychoi@email.arizona.edu>
#  #############################################
#   Setup utility packages
#  #############################################
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.15-1ubuntu1.14.04.5 unzip=6.0-9ubuntu1.5 -y )
#  #############################################
#   Setup a Syndicate account
#  #############################################
ENV HOME="/home/syndicate"
RUN useradd syndicate \
 && echo 'syndicate:docker' | chpasswd
RUN echo "syndicate ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
RUN mkdir /home/syndicate
RUN chown -R syndicate:syndicate $HOME
#  #############################################
#   build essentials
#  #############################################
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 -y )
#  #############################################
#   fskit
#  #############################################
RUN (apt-get update ;apt-get install --no-install-recommends libfuse-dev=2.9.2-4ubuntu4.14.04.1 libattr1-dev=1:2.4.47-1ubuntu1 -y )
USER syndicate
WORKDIR $HOME
RUN wget -O fskit.zip https://github.com/jcnelson/fskit/archive/master.zip
RUN unzip fskit.zip
RUN mv fskit-master fskit
WORKDIR "fskit"
RUN make
USER root
RUN make install
#  #############################################
#   syndicate
#  #############################################
RUN (apt-get update ;apt-get install --no-install-recommends protobuf-compiler=2.5.0-9ubuntu1 libprotobuf-dev=2.5.0-9ubuntu1 libcurl4-gnutls-dev=7.35.0-1ubuntu2.20 libmicrohttpd-dev=0.9.33-1 libjson0-dev=0.11-3ubuntu1.2 valgrind=1:3.10.1-1ubuntu3~14.5 cython=0.20.1+git90-g0e6e38e-1ubuntu2 python-protobuf=2.5.0-9ubuntu1 libssl-dev=1.0.1f-1ubuntu2.27 python-crypto=2.6.1-4ubuntu0.3 python-requests=2.2.1-1ubuntu0.4 -y )
USER syndicate
WORKDIR $HOME
RUN wget -O syndicate.zip https://github.com/jcnelson/syndicate/archive/master.zip
RUN unzip syndicate.zip
RUN mv syndicate-master syndicate
WORKDIR "syndicate"
#   replace localhost to $MS_HOST$
RUN sed -i 's/localhost/$MS_HOST$/g' ms/common/msconfig.py
RUN make MS_APP_ADMIN_EMAIL=$MS_APP_ADMIN_EMAIL$
RUN echo "======== SYNDICATE ADMIN INFO ========"
RUN cat build/out/ms/common/admin_info.py
USER root
RUN make -C libsyndicate install
RUN make -C libsyndicate-ug install
RUN make -C python install
WORKDIR $HOME/syndicate/gateways/acquisition
RUN make install
WORKDIR $HOME/syndicate/gateways/user
RUN make install
EXPOSE 31111/tcp
#  #############################################
#   google app engine
#  #############################################
USER syndicate
WORKDIR $HOME
RUN wget https://storage.googleapis.com/appengine-sdks/featured/google_appengine_1.9.33.zip
RUN unzip google_appengine_1.9.33.zip
EXPOSE 8080/tcp
EXPOSE 8000/tcp
#  #############################################
#   Run Syndicate MS
#  #############################################
USER syndicate
WORKDIR $HOME
RUN mkdir /home/syndicate/datastore
CMD ["/home/syndicate/google_appengine/dev_appserver.py", "--admin_host=0.0.0.0", "--host=0.0.0.0", "--storage_path=/home/syndicate/datastore", "/home/syndicate/syndicate/build/out/ms"]
# Please add your HEALTHCHECK here!!!
