FROM fluxproject/ros_base
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends locales=2.37-0ubuntu2 bzip2=1.0.8-5build1 tree=2.1.0-1 unzip=6.0-27ubuntu1 xz-utils=5.4.1-0.2 curl=7.88.1-7ubuntu1 wget=1.21.3-1ubuntu1 iproute2=6.1.0-1ubuntu2 sudo=1.9.13p1-1ubuntu2 python-pip python3-pip=23.0.1+dfsg-1 python-setuptools python3-setuptools=66.1.1-1 openjdk-8-jdk-headless=8u362-ga-0ubuntu2 nodejs=18.13.0+dfsg1-1ubuntu2 npm=9.2.0~ds1-1 nodejs-legacy iputils-ping=3:20221126-1 net-tools=2.10-0.1ubuntu3 iproute knot-dnsutils=3.2.5-1 vim=2:9.0.1000-4ubuntu2 ffmpeg=7:5.1.2-3ubuntu1 -y ) \
 && rm -rf /var/lib/apt/lists/*
#   Jupyterhub setting
RUN mkdir -p /etc/jupyterhub
COPY jupyterhub_config.py /etc/jupyterhub/
#   Introduce flux user   # TODO: link
RUN npm install configurable-http-proxy@4.5.5 -g
RUN useradd -u 11111 -m -s /bin/bash flux
RUN usermod -aG sudo flux
RUN bash -c " echo flux:flux | chpasswd "
#   Flux user setting    # TODO: link
COPY spark-ex-kubernetes.sh /home/flux/
RUN python2 -m pip install --upgrade --user pip \
 && python3 -m pip install --upgrade --user pip \
 && python3 -m pip install --no-cache-dir --upgrade jupyter jupyterhub jupyterlab \
 && python2 -m pip install --no-cache-dir --upgrade pyspark matplotlib pandas tensorflow keras Pillow \
 && python2 -m pip install --no-cache-dir --upgrade --force-reinstall requests imageio moviepy seaborn gmaps \
 && python2 -m pip install ipykernel \
 && python2 -m ipykernel install \
 && python3 -m pip install ipykernel \
 && python3 -m ipykernel install
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 66F84AE1EB71A8AC108087DCAF677210FF6D3CDA \
 && bash -c 'echo "deb [ arch=amd64 ] http://packages.dataspeedinc.com/ros/ubuntu $(lsb_release -sc) main" > /etc/apt/sources.list.d/ros-dataspeed-public.list' \
 && :
RUN bash -c 'echo "yaml http://packages.dataspeedinc.com/ros/ros-public-'$ROS_DISTRO'.yaml '$ROS_DISTRO'" > /etc/ros/rosdep/sources.list.d/30-dataspeed-public-'$ROS_DISTRO'.list' \
 && rosdep update 2> /dev/null \
 && (apt-get update ;apt-get install --no-install-recommends ros-$ROS_DISTRO-dbw-mkz ros-$ROS_DISTRO-mobility-base ros-$ROS_DISTRO-baxter-sdk ros-$ROS_DISTRO-velodyne -y ) \
 && rm -rf /var/lib/apt/lists/*
#   Default to UTF-8
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"
ENV PATH="$PATH:/opt/apache/hadoop/bin"
ENV ROSIF_JAR="/opt/ros_hadoop/master/lib/rosbaginputformat.jar"
RUN mkdir -p /opt/ros_hadoop/master/dist/
RUN mkdir -p /opt/apache/
RUN mkdir -p /opt/ros_spark/dist/
COPY . /opt/ros_hadoop/master/
#   TODO: ENV ROS_HADOOP='0.9.11'
#   RUN \
#     curl -s "https://codeload.github.com/valtech/ros_hadoop/tar.gz/v${ROS_HADOOP}" | \
#     tar -C /opt/ros_hadoop -xvzf - && \
#     mv /opt/ros_hadoop/ros_hadoop-${ROS_HADOOP} /opt/ros_hadoop/latest
RUN curl -s "https://codeload.github.com/valtech/ros_hadoop/tar.gz/master" | tar -C /opt/ros_hadoop -xvzf - \
 && mv /opt/ros_hadoop/ros_hadoop-master /opt/ros_hadoop/latest
RUN bash -c "if [ ! -f /opt/ros_hadoop/master/dist/hadoop-3.0.0.tar.gz ] ; then wget --no-check-certificate -O /opt/ros_hadoop/master/dist/hadoop-3.1.1.tar.gz -q https://www.eu.apache.org/dist/hadoop/common/hadoop-3.1.1/hadoop-3.1.1.tar.gz ; fi"
RUN tar -xzf /opt/ros_hadoop/master/dist/hadoop-3.1.1.tar.gz -C /opt/apache \
 && rm /opt/ros_hadoop/master/dist/hadoop-3.1.1.tar.gz
RUN ln -s /opt/apache/hadoop-3.1.1 /opt/apache/hadoop
RUN bash -c "if [ ! -f /opt/ros_hadoop/latest/lib/rosbaginputformat.jar ] ; then ln -s /opt/ros_hadoop/master/lib/rosbaginputformat.jar /opt/ros_hadoop/latest/lib/rosbaginputformat.jar ; fi"
#  # for spark example tests
RUN bash -c "if [ ! -f /opt/ros_spark/dist/spark-2.3.1-bin-hadoop2.7.tgz ] ; then wget --quiet -O /opt/ros_spark/dist/spark-2.3.1-bin-hadoop2.7.tgz http://apache.lauf-forum.at/spark/spark-2.3.1/spark-2.3.1-bin-hadoop2.7.tgz ; fi"
RUN tar -xzf /opt/ros_spark/dist/spark-2.3.1-bin-hadoop2.7.tgz -C /opt/apache \
 && rm /opt/ros_spark/dist/spark-2.3.1-bin-hadoop2.7.tgz
RUN printf "<configuration>\n\n<property>\n<name>fs.defaultFS</name>\n<value>hdfs://localhost:9000</value>\n</property>\n</configuration>" > /opt/apache/hadoop/etc/hadoop/core-site.xml \
 && printf "<configuration>\n<property>\n<name>dfs.replication</name>\n<value>1</value>\n</property>\n</configuration>" > /opt/apache/hadoop/etc/hadoop/hdfs-site.xml \
 && bash -c "/opt/apache/hadoop/bin/hdfs namenode -format 2>/dev/null" \
 && printf "#! /bin/bash\n/opt/apache/hadoop/bin/hdfs --daemon stop datanode\n/opt/apache/hadoop/bin/hdfs --daemon stop namenode\n/opt/apache/hadoop/bin/hdfs --daemon start namenode\n/opt/apache/hadoop/bin/hdfs --daemon start datanode\nexec \"$@\"\n" > /start_hadoop.sh \
 && chmod a+x /start_hadoop.sh
RUN printf "#! /bin/bash\nset -e\nsource \"/opt/ros/$ROS_DISTRO/setup.bash\"\n/start_hadoop.sh\nexec \"$@\"\n" > /ros_hadoop.sh \
 && chmod a+x /ros_hadoop.sh
RUN bash -c "if [ ! -f /opt/ros_hadoop/master/dist/HMB_4.bag ] ; then wget --quiet -O /opt/ros_hadoop/master/dist/HMB_4.bag https://xfiles.valtech.io/f/c494d168522045e3bcc0/?dl=1 ; fi" \
 && java -jar "$ROSIF_JAR" -f /opt/ros_hadoop/master/dist/HMB_4.bag
RUN bash -c "/start_hadoop.sh" \
 && until /opt/apache/hadoop/bin/hdfs dfsadmin -safemode wait ; do sleep 1s ; done \
 && until /opt/apache/hadoop/bin/hdfs dfsadmin -report ; do sleep 1s ; done \
 && until /opt/apache/hadoop/bin/hdfs dfs -mkdir /user ; do sleep 1s ; done \
 && /opt/apache/hadoop/bin/hdfs dfs -mkdir /user/root \
 && /opt/apache/hadoop/bin/hdfs dfs -mkdir /user/flux \
 && /opt/apache/hadoop/bin/hdfs dfs -put /opt/ros_hadoop/master/dist/HMB_4.bag \
 && /opt/apache/hadoop/bin/hdfs --daemon stop datanode \
 && /opt/apache/hadoop/bin/hdfs --daemon stop namenode
RUN mkdir -p /ope/ros_hadoop/latest/doc \
 && chmod -R 777 /opt/ros_hadoop
WORKDIR /opt/ros_hadoop/latest/doc/
ENTRYPOINT ["/ros_hadoop.sh"]
CMD ["jupyterhub", "-f", "/etc/jupyterhub/jupyterhub_config.py"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
