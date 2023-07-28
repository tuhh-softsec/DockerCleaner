#
#   Licensed to the Apache Software Foundation (ASF) under one
#   or more contributor license agreements.  See the NOTICE file
#   distributed with this work for additional information
#   regarding copyright ownership.  The ASF licenses this file
#   to you under the Apache License, Version 2.0 (the
#   "License"); you may not use this file except in compliance
#   with the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing,
#   software distributed under the License is distributed on an
#   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
#   KIND, either express or implied.  See the License for the
#   specific language governing permissions and limitations
#   under the License.
FROM ubuntu:16.04
#  ## Get necessary libraries to add postgresql apt repository
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends lsb-core=9.20160110ubuntu0.2 software-properties-common=0.96.20.10 wget=1.17.1-1ubuntu1.5 -y )
#  ## Add postgresql apt repository
RUN add-apt-repository "deb http://apt.postgresql.org/pub/repos/apt/ $( lsb_release -sc ;)-pgdg main" \
 && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
#  ## Have to update after getting new repository
RUN :
#  ## Get postgres10 and postgres specific add-ons
RUN (apt-get update ;apt-get install --no-install-recommends postgresql-10 postgresql-client-10 postgresql-plpython-10 postgresql-server-dev-10 libpq-dev=9.5.25-0ubuntu0.16.04.1 build-essential=12.1ubuntu2 openssl=1.0.2g-1ubuntu4.20 libssl-dev=1.0.2g-1ubuntu4.20 libboost-all-dev=1.58.0.1ubuntu1 m4=1.4.17-5 vim=2:7.4.1689-3ubuntu1.5 pgxnclient=1.2.1-3 flex=2.6.0-11 bison=2:3.0.4.dfsg-1 graphviz=2.38.0-12ubuntu2.1 -y )
#  ## Reset pg_hba.conf file to allow no password prompt
#  ## Docker file doesn't support heardoc, like cat > /etc/postgresql/10/main/pg_hba.conf<<-EOF,
#  ## and this echo and \n\ are workaround to write the file
RUN echo " \n local all all trust \n local all all trust \n host all all 127.0.0.1/32 trust \n host all all ::1/128 trust \n" > /etc/postgresql/10/main/pg_hba.conf
#  ## We need to set nproc to unlimited to be able to run scripts as
#  ## the user 'postgres'. This is actually useful when we try to setup
#  ## and start a Postgres server.
RUN echo " * soft nproc unlimited " > /etc/security/limits.d/postgres-limits.conf
#  ## Always start postgres server when login
RUN echo "service postgresql start" >> ~/.bashrc
#  ## Build custom CMake with SSQL support
RUN wget https://cmake.org/files/v3.6/cmake-3.6.1.tar.gz \
 && tar -zxvf cmake-3.6.1.tar.gz \
 && cd cmake-3.6.1 \
 && sed -i 's/-DCMAKE_BOOTSTRAP=1/-DCMAKE_BOOTSTRAP=1 -DCMAKE_USE_OPENSSL=ON/g' bootstrap \
 && ./configure \
 && make -j2 \
 && make install \
 && cd ..
#  ## Install doxygen-1.8.13:
RUN wget http://ftp.stack.nl/pub/users/dimitri/doxygen-1.8.13.src.tar.gz \
 && tar xf doxygen-1.8.13.src.tar.gz \
 && cd doxygen-1.8.13 \
 && mkdir build \
 && cd build \
 && cmake -G "Unix Makefiles" .. \
 && make \
 && make install
#  ## Optional: install LaTex
#  ## uncomment the following 'RUN apt-get' line to bake LaTex into the image
#  ## Note: if you run the following line, please tag the image as
#  ## madlib/postgres_10:LaTex, and don't tag it as latest
#   RUN apt-get install -y texlive-full
#  # To build an image from this docker file without LaTex, from madlib folder, run:
#  # docker build -t madlib/postgres_10:latest -f tool/docker/base/Dockerfile_ubuntu16_postgres10 .
#  # To push it to docker hub, run:
#  # docker push madlib/postgres_10:latest
#  # To build an image from this docker file with LaTex, from madlib folder, uncomment
#  # line 88, and run:
#  # docker build -t madlib/postgres_10:LaTex -f tool/docker/base/Dockerfile_ubuntu16_postgres10 .
#  # To push it to docker hub, run:
#  # docker push madlib/postgres_10:LaTex
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
