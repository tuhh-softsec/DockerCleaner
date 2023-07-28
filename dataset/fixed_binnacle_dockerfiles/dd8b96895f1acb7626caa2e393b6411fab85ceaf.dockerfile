#   Name:         Dockerfile
#   Description:  build the mercuryiss/kali docker image
#   Date:         2018-01-09
#   Author:       Alexi Chiotis - Mercury ISS
FROM kalilinux/kali-linux-docker
#   HACK: prepend AARNet mirror so as to circumvent apt-get 404s
RUN sed -i "s/https\.kali\.org/mirror\.aarnet\.edu\.au\/pub\/kali/g" /etc/apt/sources.list
#   update the archive keyring 
#   https://unix.stackexchange.com/questions/421985/invalid-signature-when-trying-to-apt-get-update-on-kali
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg=2.2.40-1ubuntu2 curl=7.88.1-7ubuntu1 -y
#  RUN wget -q -O - https://archive.kali.org/archive-key.asc  | apt-key add
RUN curl https://archive.kali.org/archive-key.asc | apt-key add
#   get faster mirrors into apt/sources.list
#  RUN apt-get update && apt-get install -y netselect-apt
#  RUN cp /etc/apt/sources.list /etc/apt/sources.list.bak
#  RUN netselect-apt
#  RUN cp sources.list /etc/apt/sources.list
#   default install is bare, install some tools 
RUN apt-get update \
 && apt-get install --no-install-recommends arachni beef-xss cutycapt=0.0~svn10-0.1build1 dirbuster exploitdb fierce=1.5.0-1 firefox-esr git=1:2.39.2-1ubuntu1 gpp-decrypt hydra=9.4-1 john=1.8.0-4ubuntu3 locate=4.9.0-3ubuntu1 man metasploit-framework nbtscan=1.7.2-2 netselect-apt nikto=1:2.1.5-3.1 nmap=7.93+dfsg1-1 python-pip responder skipfish sparta sqlitebrowser=3.12.2-1 sqlmap=1.7.3-1 unicornscan vim=2:9.0.1000-4ubuntu2 virtualenv=20.19.0+ds-1 websploit=4.0.4-3 whois=5.5.16 wordlists wpscan zenmap -y
#   install those packages that have interactive installation method
WORKDIR /tmp
#   set up powershell-empire
RUN apt-get install --no-install-recommends multiarch-support -y
RUN wget http://http.us.debian.org/debian/pool/main/o/openssl/libssl1.0.0_1.0.1t-1+deb8u7_amd64.deb
RUN dpkg -i libssl1.0.0_1.0.1t-1+deb8u7_amd64.deb
WORKDIR /usr/share
RUN git clone https://github.com/EmpireProject/Empire empire
WORKDIR empire/setup
RUN printf "Y\n\n" | ./install.sh
WORKDIR /usr/bin
RUN echo 'cd /usr/share/empire/ \
 && /usr/share/empire/empire' > empire \
 && chmod +x empire
#   use our custom .bashrc, used for different colour prompt string
COPY bashrc /root/.bashrc
#   TODO append localhost entry to container /etc/hosts for sudo
COPY hosts /etc/hosts
#   Set local timezone
RUN cp /usr/share/zoneinfo/Australia/Sydney /etc/localtime
#   accept --build-arg arguments
ARG USERNAME
ARG UID
ARG GID
#   copy these host environment variables into the container environment
ENV USERNAME="$USERNAME"
ENV UID="$UID"
ENV GID="$GID"
ENV COLUMNS="$COLUMNS"
ENV LINES="$LINES"
#   add pentest user within kali and run as non-privileged, in sudo group
#   note that if you use --net to start container, the username passed should be
#   available in the host's /etc/passwd
#   http://wiki.ros.org/docker/Tutorials/GUI 
RUN useradd -m $USERNAME \
 && echo "$USERNAME:$USERNAME" | chpasswd \
 && usermod --shell /bin/bash $USERNAME \
 && usermod -aG sudo $USERNAME \
 && echo "$USERNAME ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers.d/$USERNAME \
 && chmod 0440 /etc/sudoers.d/$USERNAME \
 && usermod --uid $UID $USERNAME \
 && groupmod --gid $GID $USERNAME
USER $USERNAME
ENV HOME="/home/$USERNAME"
WORKDIR $HOME
RUN ln -s /usr/share/wordlists/
RUN ln -s /var/www/html/ www
RUN ln -s /data
RUN ln -s /scripts
RUN ln -s /installers
COPY bashrc .bashrc
#   https://raw.githubusercontent.com/makefu/dnsmap/master/wordlist_TLAs.txt
COPY dnsmap.txt wordlists/
# Please add your HEALTHCHECK here!!!
