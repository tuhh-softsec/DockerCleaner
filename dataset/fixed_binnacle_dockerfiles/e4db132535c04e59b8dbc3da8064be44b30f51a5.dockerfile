#   Set up the basics 
#   Leverage Kali Base Image
FROM kalilinux/kali-linux-docker:latest
#   Update off the bat. Don't dist-upgrade with docker.
RUN : \
 && apt-get upgrade -y
#   Install the Core
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends sudo=1.9.13p1-1ubuntu2 git=1:2.39.2-1ubuntu1 wget=1.21.3-1ubuntu1 curl=7.88.1-7ubuntu1 git=1:2.39.2-1ubuntu1 zip=3.0-13 ccze=0.2.1-7 byobu=5.133-1.1 zsh=5.9-4 golang=2:1.20~0ubuntu1 ufw=0.36.1-4.1 python-pip nikto=1:2.1.5-3.1 dotdotpwn jsql nmap=7.93+dfsg1-1 sqlmap=1.7.3-1 sqlninja thc-ipv6=3.8-1build1 hydra=9.4-1 dirb=2.22+dfsg-5 -y )
#   Make Directory Structure
RUN mkdir /usr/share/wordlists \
 && mkdir -p /usr/share/tools/scripts/
#   Pull Wordlists
RUN git clone https://github.com/danielmiessler/SecLists /usr/share/wordlists/seclists
RUN git clone https://github.com/danielmiessler/RobotsDisallowed /usr/share/wordlists/robotsdisallowed
RUN cd /usr/share/wordlists/seclists/Passwords/Leaked-Databases \
 && tar xvzf rockyou.txt.tar.gz
#   Set up DNS Tooling
#  ## Building Aquatone
FROM ruby
WORKDIR /
#   Prepare
RUN : \
 && apt-get upgrade -y
#   Install normal Packages needed
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends apt-utils=2.6.0 unzip=6.0-27ubuntu1 nodejs=18.13.0+dfsg1-1ubuntu2 wget=1.21.3-1ubuntu1 curl=7.88.1-7ubuntu1 jruby=9.3.9.0+ds-1 nano=7.2-1 screen=4.9.0-4 htop=3.2.2-1 openssl=3.0.8-1ubuntu1 git=1:2.39.2-1ubuntu1 -y -u )
#   Install nodejs
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash -
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends nodejs=18.13.0+dfsg1-1ubuntu2 -y )
#   Installing the packages needed to run Nightmare
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends xvfb=2:21.1.7-1ubuntu2 x11-xkb-utils=7.7+7 xfonts-100dpi=1:1.0.5 xfonts-75dpi=1:1.0.5 xfonts-scalable=1:1.0.3-1.3 xfonts-cyrillic=1:1.0.5 x11-apps=7.7+9 clang=1:15.0-56~exp2 libdbus-1-dev=1.14.4-1ubuntu1 libgtk2.0-dev=2.24.33-2ubuntu2 libnotify-dev=0.8.1-1 libgnome-keyring-dev libgconf2-dev=3.2.6-8ubuntu1 libasound2-dev=1.2.8-1build1 libcap-dev=1:2.66-3ubuntu2 libcups2-dev=2.4.2-1ubuntu4 libxtst-dev=2:1.2.3-1.1 libxss1=1:1.2.3-1build2 libnss3-dev=2:3.87.1-1 gcc-multilib=4:12.2.0-3ubuntu1 g++-multilib=4:12.2.0-3ubuntu1 -y )
#   Use aq domain.com to automated all options of aquatone.
RUN wget "https://gist.githubusercontent.com/random-robbie/beae1991e9ad139c6168c385d8a31f7d/raw/" -O /bin/aq
RUN chmod 777 /bin/aq
#  install aquatone
RUN gem install aquatone --version 0.5.0
#   Clone down other DNS tooling
RUN mkdir -p /usr/share/tools/DNS
RUN git clone https://github.com/lorenzog/dns-parallel-prober /usr/share/tools/DNS/dnsq
RUN git clone https://github.com/aboul3la/Sublist3r /usr/share/tools/DNS/sublist3r
RUN git clone https://github.com/guelfoweb/knock /usr/share/tools/DNS/knock
RUN git clone https://github.com/anshumanbh/brutesubs /usr/share/tools/DNS/brutesubs
RUN git clone https://github.com/jhaddix/domain /usr/share/tools/DNS/domain
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends fierce=1.5.0-1 -y ) || true
#   CMS Tooling
RUN mkdir -p /usr/share/tools/CMS
RUN git clone https://github.com/droope/droopescan /usr/share/tools/CMS/droopscan
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends wpscan -y ) || true
RUN git clone https://github.com/Dionach/CMSmap /usr/share/tools/CMS/cmsmap
#   Directory Busting
RUN mkdir -p /usr/share/tools/DirBusting
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends dirb=2.22+dfsg-5 -y ) || true
RUN git clone https://github.com/OJ/gobuster /usr/share/tools/DirBusting/gobuster
RUN git clone https://github.com/henshin/filebuster /usr/share/tools/DirBusting/filebuster
#   Git Recon
RUN mkdir /usr/share/tools/gitint
RUN git clone https://github.com/libcrack/gitrecon /usr/share/tools/gitint/gitrecon
RUN git clone https://github.com/dxa4481/truffleHog /usr/share/tools/gitint/trufflehog
RUN git clone https://github.com/michenriksen/gitrob /usr/share/tools/gitint/gitrob
#   OSINT Tooling 
RUN mkdir -p /usr/share/tools/OSINT
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends recon-ng=5.1.2-1 -y ) || true
RUN git clone https://github.com/smicallef/spiderfoot /usr/share/tools/OSINT/spiderfoot
RUN git clone https://github.com/ZephrFish/GoogD0rker /usr/share/tools/OSINT/googd0rk
RUN git clone https://github.com/GerbenJavado/LinkFinder /usr/share/tools/OSINT/linkfinder
#   HTTP Analysis
RUN mkdir /usr/share/tools/HTTPAnal
RUN git clone https://github.com/ChrisTruncer/EyeWitness /usr/share/tools/HTTPAnal/eyewitness
RUN git clone https://github.com/robertdavidgraham/masscan /usr/share/tools/HTTPAnal/masscan
#   Optional
#   BBF Tooling (There will be some dupes SORRY!
RUN mkdir -p /usr/share/tools/BBF \
 && cd /usr/share/tools/BBF \
 && for y in $( wget https://bugbountyforum.com/tools/ \
 && grep "/tools/" index.html | cut -d "=" -f 2 | cut -d "/" -f 2,3 | grep -v ">" ;); do wget https://bugbountyforum.com/$y ; done \
 && for x in $( ls ;); do grep "href=" $x | cut -d "=" -f 2 | grep github.com | cut -d "/" -f 3,4,5 | cut -d " " -f 1 | sed -e 's/^"//' -e 's/"$//' | grep -v "gist" >> Repos.txt; done \
 && for a in $( cat Repos.txt ;); do git clone https://$a ; done \
 && find . -maxdepth 1 -type f -delete
RUN echo "That's all folks! You're good to go hack the planet!"
#   set to bash so you can set keys before running aquatone.
ENTRYPOINT ["/bin/bash"]
#    Set working directory
WORKDIR /root/
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
