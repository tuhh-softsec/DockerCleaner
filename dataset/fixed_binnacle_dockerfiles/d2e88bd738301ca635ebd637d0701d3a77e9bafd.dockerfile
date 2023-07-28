#   Ubuntu Docker file for Julia with slurm
#   Version:0.0.1
FROM ubuntu:16.04
MAINTAINER Tanmay Mohapatra
RUN apt-get update \
 && apt-get upgrade -y -o Dpkg::Options::="--force-confdef" -o DPkg::Options::="--force-confold" \
 && (apt-get update ;apt-get install --no-install-recommends man-db=2.7.5-1 libc6=2.23-0ubuntu11.3 libc6-dev=2.23-0ubuntu11.3 build-essential=12.1ubuntu2 wget=1.17.1-1ubuntu1.5 curl=7.47.0-1ubuntu2.19 file=1:5.25-2ubuntu1.4 vim=2:7.4.1689-3ubuntu1.5 screen=4.3.1-2ubuntu0.1 tmux=2.1-3build1 unzip=6.0-20ubuntu1.1 pkg-config=0.29.1-0ubuntu1 cmake=3.5.1-1ubuntu3 gfortran=4:5.3.1-1ubuntu1 gettext=0.19.7-2ubuntu3.1 libreadline-dev=6.3-8ubuntu2 libncurses-dev libpcre3-dev=2:8.38-3.1 libgnutls30=3.4.10-4ubuntu1.9 libzmq3-dev=4.1.4-7ubuntu0.1 libzmq5=4.1.4-7ubuntu0.1 python=2.7.12-1~16.04 python-yaml=3.11-3build1 python-m2crypto=0.22.6~rc4-1ubuntu1 python-crypto=2.6.1-6ubuntu0.16.04.3 msgpack-python python-dev=2.7.12-1~16.04 python-setuptools=20.7.0-1 supervisor=3.2.0-2ubuntu0.2 python-jinja2=2.8-1ubuntu0.1 python-requests=2.9.1-3ubuntu0.1 python-isodate=0.5.4-1 python-git=1.0.1+git137-gc8b8379-2.1 python-pip=8.1.1-2ubuntu0.6 -y ) \
 && apt-get clean
RUN pip install pyzmq==25.0.2 PyDrive==1.3.1 google-api-python-client==2.85.0 jsonpointer==2.3 jsonschema==4.17.3 tornado==6.2 sphinx==6.1.3 pygments==2.15.0 nose==1.3.7 readline==6.2.4.2 mistune==2.0.5 invoke==2.0.0 --upgrade
RUN pip install 'notebook==4.2'
RUN pip install 'rise==4.0.0b1' ; jupyter-nbextension install rise --py --sys-prefix ; jupyter-nbextension enable rise --py --sys-prefix
#   Install julia 0.3
RUN mkdir -p /opt/julia-0.3.12 \
 && curl -s -L https://julialang.s3.amazonaws.com/bin/linux/x64/0.3/julia-0.3.12-linux-x86_64.tar.gz | tar -C /opt/julia-0.3.12 -x -z --strip-components=1 -f -
RUN ln -fs /opt/julia-0.3.12 /opt/julia-0.3
#   Install julia 0.4
RUN mkdir -p /opt/julia-0.4.7 \
 && curl -s -L https://julialang.s3.amazonaws.com/bin/linux/x64/0.4/julia-0.4.7-linux-x86_64.tar.gz | tar -C /opt/julia-0.4.7 -x -z --strip-components=1 -f -
RUN ln -fs /opt/julia-0.4.7 /opt/julia-0.4
#   Install julia 0.5
RUN mkdir -p /opt/julia-0.5.0 \
 && curl -s -L https://julialang.s3.amazonaws.com/bin/linux/x64/0.5/julia-0.5.0-linux-x86_64.tar.gz | tar -C /opt/julia-0.5.0 -x -z --strip-components=1 -f -
RUN ln -fs /opt/julia-0.5.0 /opt/julia-0.5
#   Install julia 0.6
RUN mkdir -p /opt/julia-0.6.0-dev \
 && curl -s -L https://status.julialang.org/download/linux-x86_64 | tar -C /opt/julia-0.6.0-dev -x -z --strip-components=1 -f -
RUN ln -fs /opt/julia-0.6.0-dev /opt/julia-0.6
#   Make v0.5 default julia
RUN ln -fs /opt/julia-0.5 /opt/julia
#   Setup juliarc
RUN echo '("JULIA_LOAD_CACHE_PATH" in keys(ENV)) \
 && unshift!(Base.LOAD_CACHE_PATH, ENV["JULIA_LOAD_CACHE_PATH"])' >> /opt/julia-0.4/etc/julia/juliarc.jl
RUN echo '("JULIA_LOAD_CACHE_PATH" in keys(ENV)) \
 && unshift!(Base.LOAD_CACHE_PATH, ENV["JULIA_LOAD_CACHE_PATH"])' >> /opt/julia-0.5/etc/julia/juliarc.jl
RUN echo '("JULIA_LOAD_CACHE_PATH" in keys(ENV)) \
 && unshift!(Base.LOAD_CACHE_PATH, ENV["JULIA_LOAD_CACHE_PATH"])' >> /opt/julia-0.6/etc/julia/juliarc.jl
RUN echo "PATH=\"/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/opt/julia/bin\"" > /etc/environment \
 && echo "export PATH" >> /etc/environment \
 && echo "source /etc/environment" >> /root/.bashrc
ENV SLURM_VER="16.05.6"
#   Create users, set up SSH keys (for MPI)
RUN useradd -u 2001 -d /home/slurm slurm
RUN useradd -u 6000 -ms /bin/bash juser
COPY etc/sudoers.d/juser /etc/sudoers.d/juser
COPY home/juser/ssh/config /home/juser/.ssh/config
COPY home/juser/ssh/id_rsa /home/juser/.ssh/id_rsa
COPY home/juser/ssh/id_rsa.pub /home/juser/.ssh/id_rsa.pub
COPY home/juser/ssh/authorized_keys /home/juser/.ssh/authorized_keys
RUN chown -R juser:juser /home/juser/.ssh/
RUN chmod 400 /home/juser/.ssh/*
#   Install packages
RUN : \
 && apt-get -y dist-upgrade
RUN (apt-get update ;apt-get install --no-install-recommends munge=0.5.11-3ubuntu0.1 curl=7.47.0-1ubuntu2.19 gcc=4:5.3.1-1ubuntu1 make=4.1-6 bzip2=1.0.6-8ubuntu0.2 supervisor=3.2.0-2ubuntu0.2 python=2.7.12-1~16.04 python-dev=2.7.12-1~16.04 libmunge-dev=0.5.11-3ubuntu0.1 libmunge2=0.5.11-3ubuntu0.1 lua5.3=5.3.1-1ubuntu2.1 lua5.3-dev libopenmpi-dev=1.10.2-8ubuntu1 openmpi-bin=1.10.2-8ubuntu1 gfortran=4:5.3.1-1ubuntu1 vim=2:7.4.1689-3ubuntu1.5 python-mpi4py=1.3.1+hg20131106-2ubuntu5 python-numpy=1:1.11.0-1ubuntu1 python-psutil=3.4.2-1ubuntu0.1 sudo=1.8.16-0ubuntu1.10 psmisc=22.21-2.1ubuntu0.1 software-properties-common=0.96.20.10 python-software-properties=0.96.20.10 iputils-ping=3:20121221-5ubuntu2 openssh-server=1:7.2p2-4ubuntu2.10 openssh-client=1:7.2p2-4ubuntu2.10 -y )
#   Download, compile and install SLURM
RUN curl -fsL http://www.schedmd.com/download/total/slurm-${SLURM_VER}.tar.bz2 | tar xfj - -C /opt/ \
 && cd /opt/slurm-${SLURM_VER}/ \
 && ./configure \
 && make \
 && make install
COPY etc/slurm/slurm.conf /usr/local/etc/slurm.conf
#   Configure OpenSSH
#   Also see: https://docs.docker.com/engine/examples/running_ssh_service/
ENV NOTVISIBLE="\"in users profile\""
RUN echo "export VISIBLE=now" >> /etc/profile
RUN mkdir /var/run/sshd
RUN echo 'juser:juser' | chpasswd
#   SSH login fix. Otherwise user is kicked off after login
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd
COPY etc/supervisord.d/sshd.conf /etc/supervisor/conf.d/sshd.conf
#   Configure munge (for SLURM authentication)
COPY etc/munge/munge.key /etc/munge/munge.key
RUN mkdir /var/run/munge \
 && chown root /var/lib/munge \
 && chown root /etc/munge \
 && chmod 600 /var/run/munge \
 && chmod 755 /run/munge \
 && chmod 600 /etc/munge/munge.key
COPY etc/supervisord.d/munged.conf /etc/supervisor/conf.d/munged.conf
RUN /opt/julia/bin/julia -e 'Pkg.clone("https://github.com/JuliaParallel/Slurm.jl.git")'
RUN /opt/julia/bin/julia -e 'Pkg.add("IJulia")'
RUN /opt/julia/bin/julia -e 'Pkg.build("IJulia")'
RUN /opt/julia/bin/julia -e 'import IJulia; import Slurm'
EXPOSE 22/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
