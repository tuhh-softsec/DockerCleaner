#
#  Build Nexus/LISP docker image based on a Debian Linux kernel. This
#  Dockerfile is for the Tritium release of the TAO. The docker image name
#  should be called "tritium". To build, use the "docker build" command in
#  the LLL-TAO git repo directory:
#
#      cd LLL-TAO
#      docker build -t tritium .
#
#  If you don't have docker on your MacOS system, download it at:
#
#      https://download.docker.com/mac/stable/Docker.dmg
#
#  Once you install it you can run docker commands in a Terminal window.
#
#  For instructions on how to use this Dockerfile, see ./docs/how-to-docker.md.
#
#
#  We prefer Debian as a base OS.
#
FROM debian:latest
#
#  Get latest lispers.net release from Dropbox.
#
ENV LISP_URL="https://www.dropbox.com/s/e87heamhl9t5asz/lisp-nexus.tgz"
#
#  Install tools we need for a networking geek.
#
RUN apt-get update \
 && apt-get install gcc libc-dev python python-dev libffi-dev openssl libpcap-dev curl wget iptables iproute2 tcpdump tcsh sudo traceroute iputils-ping net-tools procps emacs jq -yq
#
#  Install Nexus dependencies.
#
RUN apt-get update \
 && apt-get install build-essential libdb++-dev libssl1.0-dev -yq
#
#  Install LISP release in /lispers.net directory.
#
RUN mkdir /lispers.net ; cd /lispers.net ; curl --insecure -L $LISP_URL | gzip -dc | tar -xf -
#
#  Install python modules the lispers.net directory depends on.
#
RUN python /lispers.net/get-pip.py
RUN pip install -r /lispers.net/pip-requirements.txt
#
#  Put user in the /lispers.net directory when you attach to container.
#
EXPOSE 8080/tcp
WORKDIR /lispers.net
#
#  Put Nexus source-tree in docker image and build it..
#
RUN mkdir /nexus
RUN mkdir /nexus/build
COPY ./makefile.cli /nexus
COPY ./src /nexus/src/
RUN cd /nexus ; make -f makefile.cli clean
RUN cd /nexus ; make -j 8 -f makefile.cli STATIC=1
#
#  Copy Nexus startup files.
#
# COPY config/nexus.conf /root/.Nexus/nexus.conf
COPY config/nexus.conf /root/.TAO/nexus.conf
COPY config/run-nexus /nexus/run-nexus
COPY config/curl-nexus /nexus/curl-nexus
#
#  Copy LISP startup config.
#
COPY lisp/RL /lispers.net/RL
COPY lisp/RL-seed /lispers.net/RL-seed
COPY lisp/lisp.config.xtr /lispers.net/lisp.config.xtr
COPY lisp/lisp-join.py /lispers.net/lisp-join.py
COPY lisp/make-crypto-eid.py /lispers.net/make-crypto-eid.py
#
#  Add some useful tcsh alias commands.
#
COPY config/.aliases /root/.aliases
COPY config/.cshrc /root/.cshrc
#
#  Startup lispers.net and nexus. Output some useful data and drop into tcsh.
#
ENV RUN_LISP="/lispers.net/RL"
# ENV RUN_LISP    /lispers.net/RL-seed
ENV RUN_NEXUS="/nexus/run-nexus"
ENV RUN_GETINFO="/nexus/nexus -test getinfo"
ENV RUN_PSLISP="/lispers.net/pslisp"
CMD echo "Starting LISP ..." ; $RUN_LISP ; echo "Network coming up ..." ; sleep 2 ; echo "Starting Nexus ..." ; $RUN_NEXUS ; sleep 1 ; $RUN_PSLISP ; tcsh
