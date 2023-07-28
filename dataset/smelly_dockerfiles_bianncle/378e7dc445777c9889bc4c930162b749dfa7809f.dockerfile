#  This Dockerfile is used to build an image containing basic stuff to be used as a Jenkins slave build node.
FROM ubuntu:18.04
LABEL Description=" This image is for building Jenkins Slave with Azure CLi and Ansible"
USER root
#  Make sure apt is happy
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update -y
#  Install a basic SSH server
RUN apt-get install openssh-server -y
RUN mkdir -p /var/run/sshd
#  Install JDK 8 (latest edition) for Jenkins
RUN apt-get install --no-install-recommends default-jdk -y
#  Install utilities
RUN apt-get install git -y
RUN apt-get install wget -y
RUN apt-get install build-essential -y
#  Install Other tools
RUN apt-get install virtualenv -y
RUN apt-get install bc -y
RUN apt-get install sudo -y
RUN apt-get install iputils-ping -y
RUN apt-get install picocom -y
RUN apt-get install curl -y
RUN apt-get install software-properties-common -y
RUN add-apt-repository ppa:ubuntu-toolchain-r/test -y
RUN apt-get update -y
#  Add user jenkins to the image
RUN adduser --quiet jenkins
RUN adduser jenkins dialout
RUN adduser jenkins plugdev
#  Add user jenkins to sudoers with NOPASSWD
RUN echo "jenkins ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
#  Set password for the jenkins user (you may want to alter this).
RUN echo "jenkins:jenkins" | chpasswd
#  Setting for sshd
RUN sed -i 's|session required pam_loginuid.so|session optional pam_loginuid.so|g' /etc/pam.d/sshd
#  Standard SSH port
EXPOSE 22/tcp
#  Clean up
RUN apt-get clean
#  Install Ansbile 
RUN echo "===> Adding Ansible's PPA..." \
 && apt-get update -y \
 && apt-get install gnupg2 -y \
 && echo "deb http://ppa.launchpad.net/ansible/ansible/ubuntu bionic main" | tee /etc/apt/sources.list.d/ansible.list \
 && echo "deb-src http://ppa.launchpad.net/ansible/ansible/ubuntu bionic main" | tee -a /etc/apt/sources.list.d/ansible.list \
 && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 7BB9C367 \
 && DEBIAN_FRONTEND=noninteractive apt-get update \
 && echo "===> Installing Ansible..." \
 && apt-get install ansible -y \
 && echo "===> Installing handy tools (not absolutely required)..." \
 && apt-get install python-pip -y \
 && pip install pycrypto pywinrm --upgrade \
 && apt-get install sshpass openssh-client -y \
 && echo "===> Removing Ansible PPA..." \
 && rm -rf /var/lib/apt/lists/* /etc/apt/sources.list.d/ansible.list \
 && echo "===> Adding hosts for convenience..." \
 && echo 'localhost' > /etc/ansible/hosts
#  Install Azure CLi and Dependency
RUN apt-get update -y
RUN apt-get install curl -y
RUN apt-get install build-essential libssl-dev libffi-dev python-dev -y
RUN apt-get install python-dev -y
RUN apt-get install python3-pip -y
RUN apt-get install python3 -y
RUN apt-get install python-setuptools -y
RUN curl -L https://aka.ms/InstallAzureCliBundled -o azure-cli_bundle.tar.gz
RUN tar -xvzf azure-cli_bundle.tar.gz
RUN azure-cli_bundle_*/installer
RUN echo 'export PATH=/root/bin/az:$PATH' >> ~/.bash_profile
RUN echo 'export PATH=/root/bin/az:$PATH' >> ~/.bashrc
RUN echo 'export PATH=/root/bin/az:$PATH' >> ~/.profile
RUN export PATH=$PATH:/root/bin
# Install Azure COPY 
FROM microsoft/dotnet:2.1-runtime
RUN apt-get update \
 && apt-get install --no-install-recommends rsync libunwind8 -y \
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
