#   This Dockerfile is used to build an image containing basic stuff to be used as a Jenkins slave build node.
FROM ubuntu:18.04
LABEL Description=" This image is for building Jenkins Slave with Azure CLi and Ansible"
USER root
#   Make sure apt is happy
ENV DEBIAN_FRONTEND="noninteractive"
RUN :
#   Install a basic SSH server
RUN (apt-get update ;apt-get install --no-install-recommends openssh-server=1:7.6p1-4ubuntu0.7 -y )
RUN mkdir -p /var/run/sshd
#   Install JDK 8 (latest edition) for Jenkins
RUN (apt-get update ;apt-get install --no-install-recommends default-jdk=2:1.11-68ubuntu1~18.04.1 -y )
#   Install utilities
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.17.1-1ubuntu0.17 -y )
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.19.4-1ubuntu2.2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=12.4ubuntu1 -y )
#   Install Other tools
RUN (apt-get update ;apt-get install --no-install-recommends virtualenv=15.1.0+ds-1.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends bc=1.07.1-2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends sudo=1.8.21p2-3ubuntu1.5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends iputils-ping=3:20161105-1ubuntu3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends picocom=2.2-2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 -y )
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 -y )
RUN add-apt-repository ppa:ubuntu-toolchain-r/test -y
RUN :
#   Add user jenkins to the image
RUN adduser --quiet jenkins
RUN adduser jenkins dialout
RUN adduser jenkins plugdev
#   Add user jenkins to sudoers with NOPASSWD
RUN echo "jenkins ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
#   Set password for the jenkins user (you may want to alter this).
RUN echo "jenkins:jenkins" | chpasswd
#   Setting for sshd
RUN sed -i 's|session required pam_loginuid.so|session optional pam_loginuid.so|g' /etc/pam.d/sshd
#   Standard SSH port
EXPOSE 22/tcp
#   Clean up
RUN apt-get clean
#   Install Ansbile 
RUN echo "===> Adding Ansible's PPA..." \
 && apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends gnupg2=2.2.4-1ubuntu1.6 -y ) \
 && echo "deb http://ppa.launchpad.net/ansible/ansible/ubuntu bionic main" | tee /etc/apt/sources.list.d/ansible.list \
 && echo "deb-src http://ppa.launchpad.net/ansible/ansible/ubuntu bionic main" | tee -a /etc/apt/sources.list.d/ansible.list \
 && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 7BB9C367 \
 && DEBIAN_FRONTEND=noninteractive apt-get update \
 && echo "===> Installing Ansible..." \
 && (apt-get update ;apt-get install --no-install-recommends ansible=2.5.1+dfsg-1ubuntu0.1 -y ) \
 && echo "===> Installing handy tools (not absolutely required)..." \
 && (apt-get update ;apt-get install --no-install-recommends python-pip=9.0.1-2.3~ubuntu1.18.04.8 -y ) \
 && pip install pycrypto==2.6.1 pywinrm==0.4.3 --upgrade \
 && (apt-get update ;apt-get install --no-install-recommends sshpass=1.06-1 openssh-client=1:7.6p1-4ubuntu0.7 -y ) \
 && echo "===> Removing Ansible PPA..." \
 && rm -rf /var/lib/apt/lists/* /etc/apt/sources.list.d/ansible.list \
 && echo "===> Adding hosts for convenience..." \
 && echo 'localhost' > /etc/ansible/hosts
#   Install Azure CLi and Dependency
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 -y )
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=12.4ubuntu1 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libffi-dev=3.2.1-8 python-dev=2.7.15~rc1-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python-dev=2.7.15~rc1-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python3-pip=9.0.1-2.3~ubuntu1.18.04.8 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python3=3.6.7-1~18.04 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python-setuptools=39.0.1-2ubuntu0.1 -y )
RUN curl -L https://aka.ms/InstallAzureCliBundled -o azure-cli_bundle.tar.gz
RUN tar -xvzf azure-cli_bundle.tar.gz
RUN azure-cli_bundle_*/installer
RUN echo 'export PATH=/root/bin/az:$PATH' >> ~/.bash_profile
RUN echo 'export PATH=/root/bin/az:$PATH' >> ~/.bashrc
RUN echo 'export PATH=/root/bin/az:$PATH' >> ~/.profile
RUN export PATH=$PATH:/root/bin
#  Install Azure COPY 
FROM microsoft/dotnet:2.1-runtime
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends rsync=3.1.2-2.1ubuntu1.6 libunwind8=1.2.1-8ubuntu0.1 -y ) \
 && rm -rf /var/lib/apt/lists/*
RUN set -ex \
 && curl -L -o azcopy.tar.gz https://aka.ms/downloadazcopylinux64 \
 && tar -xf azcopy.tar.gz \
 && rm -f azcopy.tar.gz \
 && ./install.sh \
 && rm -f install.sh \
 && rm -rf azcopy
CMD ["ansible-playbook", "--version"]
CMD ["python3", "--version"]
CMD ["az", "--version"]
CMD ["/usr/sbin/sshd", "-D"]
CMD ["/usr/bin/azcopy"]
CMD ["azcopy", "--version"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
