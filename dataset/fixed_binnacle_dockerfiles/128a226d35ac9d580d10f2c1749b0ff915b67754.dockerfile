FROM ubuntu:16.04
RUN useradd jenkins -u 1500 -g root
RUN : \
 && DEBIAN_FRONTEND=noninteractive apt-get install -y build-essential m4 apt-utils libffi-dev libssl-dev libbz2-dev libgmp3-dev libev-dev libsnappy-dev libxen-dev help2man pkg-config time aspcud wget rsync darcs git unzip protobuf-compiler libgcrypt20-dev libjerasure-dev yasm automake python-dev python-pip debhelper psmisc strace curl g++ libgflags-dev sudo libtool libboost-all-dev fuse sysstat ncurses-dev liburiparser1 libzmq5 librabbitmq4 tzdata
#   install specific orocksdb
RUN git clone https://github.com/domsj/orocksdb.git \
 && cd orocksdb \
 && git checkout tags/0.3.0 \
 && ./install_rocksdb.sh
RUN wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh
ENV ocaml_version="4.04.2"
RUN sh ./opam_installer.sh /usr/local/bin ${ocaml_version}
ENV opam_root="/home/jenkins/OPAM"
ENV opam_env="opam config env --root=${opam_root}"
RUN opam init --root=${opam_root} --comp ${ocaml_version}
RUN eval `${opam_env} ` \
 && opam repo add compat -k git https://github.com/toolslive/opam_anti_revisionism.git \
 && opam update -v \
 && opam install -y oasis.0.4.10 ocamlfind omake.0.9.8.7 ssl.0.5.3 camlbz2 snappy sexplib bisect lwt_ssl.1.1.0 lwt.3.0.0 camltc.0.9.4 ocplib-endian.1.0 ctypes ctypes-foreign uuidm zarith mirage-no-xen.1 quickcheck.1.0.2 ounit.2.0.0 cmdliner conf-libev depext kinetic-client cryptokit tiny_json.1.1.4 ppx_deriving.4.1 ppx_deriving_yojson base.v0.9.3 core.v0.9.1 redis.0.3.3 uri.1.9.4 piqi result ezxmlm
RUN wget https://01.org/sites/default/files/downloads/intelr-storage-acceleration-library-open-source-version/isa-l-2.14.0.tar.gz \
 && tar xfzv isa-l-2.14.0.tar.gz \
 && cd isa-l-2.14.0 \
 && ./autogen.sh \
 && ./configure \
 && make \
 && make install \
 && cd .. \
 && rm -rf isa-l-2.14.0
#   c++
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libgtest-dev=1.7.0-4ubuntu1 cmake=3.5.1-1ubuntu3 -y )
RUN cd /usr/src/gtest \
 && cmake . \
 && make \
 && mv libg* /usr/lib/
#   install specific arakoon.
RUN git clone https://github.com/openvstorage/arakoon.git
RUN cd arakoon \
 && git pull \
 && git checkout tags/1.9.22
RUN cd arakoon \
 && eval `${opam_env} ` \
 && make
RUN cd arakoon \
 && eval `${opam_env} ` \
 && export PREFIX=${opam_root}/${ocaml_version} \
 && export OCAML_LIBDIR=`ocamlfind printconf destdir ` \
 && make install
#   install orocksdb
RUN eval `${opam_env} ` \
 && cd orocksdb \
 && make build install
#  for now, install ordma manually
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends librdmacm-dev=1.0.21-1 -y )
RUN git clone https://github.com/toolslive/ordma.git \
 && cd ordma \
 && git checkout tags/0.0.2 \
 && eval `${opam_env} ` \
 && make install
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libboost1.58-all-dev=1.58.0+dfsg-5ubuntu3.1 libboost1.58-all -y --allow-unauthenticated )
RUN echo "deb http://apt.openvstorage.org unstable main" > /etc/apt/sources.list.d/ovsaptrepo.list
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends clang-3.5=1:3.5.2-3ubuntu1 liblttng-ust0=2.7.1-1 librdmacm1=1.0.21-1 libtokyocabinet9=1.4.48-10 libstdc++6:amd64 librabbitmq1 libomnithread3c2=4.1.6-2ubuntu1 libomniorb4-1=4.1.6-2ubuntu1 libhiredis0.13=0.13.3-2 liblz4-1=0.0~r131-2ubuntu2 libxio-dev libxio0 omniorb-nameserver=4.1.6-2ubuntu1 libunwind8-dev=1.1-4.1 libaio1=0.3.110-2 libaio1-dbg=0.3.110-2 libaio-dev=0.3.110-2 libz-dev libbz2-dev=1.0.6-8ubuntu0.2 libgoogle-glog-dev=0.3.4-0.1 libibverbs-dev=1.1.8-1.1ubuntu2 -y --allow-unauthenticated )
#   Install etcd:
RUN curl -L https://github.com/coreos/etcd/releases/download/v2.2.4/etcd-v2.2.4-linux-amd64.tar.gz -o etcd-v2.2.4-linux-amd64.tar.gz
RUN tar xzvf etcd-v2.2.4-linux-amd64.tar.gz
RUN cp ./etcd-v2.2.4-linux-amd64/etcd /usr/bin \
 && cp ./etcd-v2.2.4-linux-amd64/etcdctl /usr/bin
#  # installing voldrv packages only works from within the OVS LAN
ARG INSTALL_VOLDRV_PACKAGES=false
RUN test ${INSTALL_VOLDRV_PACKAGES} = 'true' || echo 'we are NOT going to install the voldrv packages/testers'
#   http://10.100.129.100:8080/view/volumedriver/view/ubuntu/job/volumedriver-no-dedup-release-ubuntu-16.04/27/artifact/volumedriver-core/build/debian/volumedriver-no-dedup-base_6.10.0-0_amd64.deb
ENV voldrv_jenkins="http://10.100.129.100:8080/view/volumedriver/view/ubuntu/job/volumedriver-no-dedup-release-ubuntu-16.04/27/artifact/volumedriver-core/build/debian"
ENV voldrv_base_pkg_name="volumedriver-no-dedup"
ENV voldrv_version="6.10.0-0_amd64"
ENV wget_mods="--timeout=10 --tries=2"
RUN test ${INSTALL_VOLDRV_PACKAGES} = 'false' || (wget ${wget_mods} ${voldrv_jenkins}/${voldrv_base_pkg_name}-base_${voldrv_version}.deb \
 && dpkg -i ${voldrv_base_pkg_name}-base_${voldrv_version}.deb )
RUN test ${INSTALL_VOLDRV_PACKAGES} = 'false' || (wget ${wget_mods} ${voldrv_jenkins}/${voldrv_base_pkg_name}-pitreplication_${voldrv_version}.deb \
 && dpkg -i ${voldrv_base_pkg_name}-pitreplication_${voldrv_version}.deb )
RUN test ${INSTALL_VOLDRV_PACKAGES} = 'false' || (wget ${wget_mods} ${voldrv_jenkins}/${voldrv_base_pkg_name}-server_${voldrv_version}.deb \
 && dpkg -i ${voldrv_base_pkg_name}-server_${voldrv_version}.deb )
RUN test ${INSTALL_VOLDRV_PACKAGES} = 'false' || (wget ${wet_mods} ${voldrv_jenkins}/${voldrv_base_pkg_name}-test_${voldrv_version}.deb \
 && dpkg -i ${voldrv_base_pkg_name}-test_${voldrv_version}.deb )
RUN pip install fabric==3.0.0 junit-xml==1.9
RUN chmod ugoa+rxw -R ${opam_root}
RUN su - -c "echo 'eval `${opam_env} `' >> /home/jenkins/.profile"
RUN su - -c "echo 'LD_LIBRARY_PATH=/usr/local/lib; export LD_LIBRARY_PATH;' >> /home/jenkins/.profile"
RUN su - -c "echo 'VOLDRV_TEST=volumedriver_test; export VOLDRV_TEST;' >> /home/jenkins/.profile"
RUN su - -c "echo 'VOLDRV_BACKEND_TEST=backend_test; export VOLDRV_BACKEND_TEST;' >> /home/jenkins/.profile"
RUN echo "jenkins ALL=NOPASSWD: ALL" > /etc/sudoers.d/jenkins
ENV ALBA_TEST="false"
ENTRYPOINT ["/bin/bash", "-c", "set", "-e", "&&", "/home/jenkins/alba/docker/docker-entrypoint.sh", "$@"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
