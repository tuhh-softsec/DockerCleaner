FROM kalilinux/kali-linux-docker
#  #######
#   INIT #
#  #######
ENV DEBIAN_FRONTEND="noninteractive"
ENV INITRD="No"
#   Term
ENV TERM="xterm"
#   Packages
RUN echo 'deb http://old.kali.org/kali sana main non-free contrib' >> /etc/apt/sources.list
RUN :
#   Locale
RUN (apt-get update ;apt-get install --no-install-recommends locales=2.37-0ubuntu2 -y ) \
 && sed -i -e 's/\# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/g' /etc/locale.gen \
 && locale-gen
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
ENV LC_CTYPE="en_US.UTF-8"
#   Utils
RUN (apt-get update ;apt-get install --no-install-recommends vim=2:9.0.1000-4ubuntu2 curl=7.88.1-7ubuntu1 silversearcher-ag=2.2.0+git20200805-1 apt-file=3.3 git=1:2.39.2-1ubuntu1 zsh=5.9-4 ncdu=1.18-0.2 -y ) \
 && git clone --depth=1 https://github.com/ston3o/oh-my-zsh ~/.oh-my-zsh \
 && cp ~/.oh-my-zsh/templates/zshrc.zsh-template ~/.zshrc \
 && sed -i -e 's/ZSH_THEME="robbyrussell"/ZSH_THEME="red"/g' ~/.zshrc \
 && apt-file update
#   Dependencies
RUN (apt-get update ;apt-get install --no-install-recommends zenity=3.44.0-1 mingw32 xterm=379-1ubuntu1 gnome-terminal=3.47.92-1ubuntu1 default-jre=2:1.17-74 default-jdk=2:1.17-74 aapt=1:10.0.0+r36-10 dex2jar zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 libmagickwand-dev=8:6.9.11.60+dfsg-1.6 imagemagick=8:6.9.11.60+dfsg-1.6 zipalign=1:10.0.0+r36-1 cowpatty=4.8-3build1 bully=1.4.00-2ubuntu1 lighttpd=1.4.67-1ubuntu2 macchanger=1.7.0-5.4 php-cgi=2:8.1+92ubuntu1 isc-dhcp-server=4.4.3-P1-1ubuntu1 python3-dev=3.11.2-1 python3-setuptools=66.1.1-1 python-pip libssl-dev=3.0.8-1ubuntu1 xprobe2 golang-go=2:1.20~0ubuntu1 whatweb=0.5.5-1 sslyze sslscan=2.0.7-1build1 wafw00f=2.2.0-1 dnsenum=1.3.1-1 -y ) \
 && (apt-get update ;apt-get install --no-install-recommends wine=8.0~repack-4 -y ) \
 && easy_install3 pip \
 && git clone --depth=1 https://github.com/noxxi/p5-io-socket-ssl \
 && cd p5-io-socket-ssl \
 && perl Makefile.PL \
 && make \
 && make install \
 && rm -r /p5-io-socket-ssl
#   MOTD
RUN echo '[ ! -z "$TERM" -a -r /etc/motd ] \
 && cat /etc/motd' >> ~/.zshrc \
 && (apt-get update ;apt-get install --no-install-recommends figlet=2.2.5-3 -y ) \
 && figlet -f small "HACKLAB" > /etc/motd \
 && sed -i '$ d' /etc/motd \
 && echo "https://github.com/ston3o/docker-hacklab" >> /etc/motd \
 && echo '\n' >> /etc/motd
#   Cheats
RUN pip install cheat==2.5.1
RUN echo "_cmpl_cheat() {\n reply=($( cheat -l | cut -d' ' -f1 ;))\n}\ncompctl -K _cmpl_cheat cheat\n" >> /root/.zshrc
RUN (apt-get update ;apt-get install --no-install-recommends nodejs-legacy npm=9.2.0~ds1-1 -y ) \
 && npm cache clean -f \
 && npm install n@9.1.0 -g \
 && n stable \
 && npm install Brosec@1.5.0 -g
#   Add files
COPY .cheat/ /root/.cheat/
COPY .vimrc /root/
COPY wordlists /usr/share/
COPY README.md /root/
COPY Dockerfile /root/
COPY cyberchef.html /root/
COPY bin/* /usr/local/bin/
WORKDIR /root
#  ################
#   INSTALL TOOLS #
#  ################
#   Footprinting / Information-Gathering / OSINT / Fingerprint
RUN (apt-get update ;apt-get install --no-install-recommends whois=5.5.16 dnsutils=1:9.18.12-1ubuntu1 dnsmap=0.36-3 nmap=7.93+dfsg1-1 theharvester dmitry=1.3a-1.2 knockpy=6.1.0-1 netdiscover=0.10-2 fierce=1.5.0-1 -y )
RUN git clone --depth=1 https://github.com/1N3/Sn1per /opt/Sn1per \
 && cd /opt/Sn1per \
 && chmod +x install.sh \
 && ./install.sh
RUN curl -o /usr/local/bin/googler https://raw.githubusercontent.com/jarun/googler/v2.3/googler \
 && chmod +x /usr/local/bin/googler
RUN pip install shodan==1.28.0
RUN git clone --depth=1 https://github.com/maurosoria/dirsearch /opt/dirsearch \
 && git clone --depth=1 https://github.com/penafieljlm/inquisitor /opt/inquisitor \
 && git clone --depth=1 https://github.com/maldevel/EmailHarvester /opt/EmailHarvester \
 && git clone --depth=1 https://github.com/killswitch-GUI/SimplyEmail/ /opt/SimplyEmail \
 && git clone --depth=1 https://github.com/HatBashBR/ShodanHat /opt/ShodanHat \
 && git clone --depth=1 https://github.com/graniet/operative-framework /opt/operative-framework \
 && git clone --depth=1 https://github.com/m4ll0k/Infoga /opt/Infoga \
 && git clone --depth=1 https://github.com/kahunalu/pwnbin /opt/pwnbin \
 && git clone --depth=1 https://github.com/vesche/scanless /opt/scanless \
 && git clone --depth=1 https://github.com/techgaun/github-dorks /opt/github-dorks \
 && git clone --depth=1 https://github.com/aboul3la/Sublist3r /opt/Sublist3r \
 && git clone --depth=1 https://github.com/subfinder/subfinder \
 && git clone --depth=1 https://github.com/TheRook/subbrute /opt/subbrute \
 && git clone --depth=1 https://github.com/utiso/dorkbot /opt/dorkbot \
 && git clone --depth=1 https://github.com/DataSploit/datasploit /opt/datasploit \
 && git clone --depth=1 https://github.com/internetwache/GitTools /opt/GitTools \
 && git clone --depth=1 https://github.com/aancw/Belati /opt/Belati
RUN (apt-get update ;apt-get install --no-install-recommends metagoofil -y )
#   Pentest Framework
#   RUN pip install pwntools
RUN (apt-get update ;apt-get install --no-install-recommends metasploit-framework websploit=4.0.4-3 -y ) \
 && git clone --depth=1 https://github.com/x3omdax/PenBox /opt/PenBox \
 && git clone --depth=1 https://github.com/golismero/golismero /opt/golismero \
 && cd /opt/golismero \
 && pip install -r requirements.txt \
 && git clone --depth=1 https://github.com/C0reL0ader/EaST /opt/EaST \
 && git clone --depth=1 https://github.com/leviathan-framework/leviathan /opt/leviathan \
 && git clone --depth=1 https://github.com/Ekultek/Pybelt /opt/Pybelt \
 && git clone --depth=1 https://github.com/nccgroup/redsnarf /opt/redsnarf \
 && git clone --depth=1 https://github.com/FreelancePentester/ddos-script /opt/ddos-script \
 && git clone --depth=1 https://github.com/viraintel/OWASP-Nettacker /opt/OWASP-Nettacker \
 && git clone --depth=1 https://github.com/Tuhinshubhra/RED_HAWK /opt/RED_HAWK \
 && git clone --depth=1 https://github.com/Manisso/fsociety /opt/fsociety
#   MITM / ARP poisoning / Spoofing / Sniffing
RUN (apt-get update ;apt-get install --no-install-recommends mitmproxy=8.1.1-1 dsniff=2.4b1+debian-31 mitmf tcpdump=4.99.3-1ubuntu1 ngrep=1.47+ds1-5 -y )
RUN git clone --depth=1 https://github.com/r00t-3xp10it/morpheus /opt/morpheus \
 && git clone --depth=1 https://github.com/evilsocket/bettercap /opt/bettercap \
 && git clone --depth=1 https://github.com/brannondorsey/mitm-router /opt/mitm-router \
 && git clone --depth=1 https://github.com/LionSec/xerosploit /opt/xerosploit
#   Email Spoofing
RUN (apt-get update ;apt-get install --no-install-recommends sendemail=1.56-5.1 -y )
RUN git clone --depth=1 https://github.com/BishopFox/spoofcheck /opt/spoofcheck
#   Crack password
#   RUN git clone --depth=1 https://github.com/berzerk0/Probable-Wordlists /opt/Probable-Wordlists
RUN (apt-get update ;apt-get install --no-install-recommends hydra=9.4-1 john=1.8.0-4ubuntu3 crunch=3.6-3 cewl=5.5.2-2 fcrackzip=1.0-11 -y )
RUN git clone --depth=1 https://github.com/Mebus/cupp /opt/cupp \
 && git clone --depth=1 https://github.com/k4m4/dymerge /opt/dymerge \
 && git clone --depth=1 https://github.com/AlessandroZ/LaZagne /opt/LaZagne \
 && git clone --depth=1 https://github.com/LandGrey/pydictor /opt/pydictor \
 && git clone --depth=1 https://github.com/UltimateHackers/Hash-Buster /opt/Hash-Buster \
 && git clone --depth=1 https://github.com/lightos/credmap /opt/credmap
#   Hash Manipulation
RUN git clone --depth=1 https://github.com/ekultek/dagon /opt/dagon \
 && gem install hashdata --version 0.0.3
#   Vulnerability Scanner (SQL, XSS, LFI, RFI etc...)
RUN (apt-get update ;apt-get install --no-install-recommends arachni nikto=1:2.1.5-3.1 wapiti=3.0.4+dfsg-2 w3af fimap -y )
RUN git clone --depth=1 https://github.com/m4ll0k/Spaghetti /opt/Spaghetti \
 && git clone --depth=1 https://github.com/faizann24/XssPy /opt/XssPy \
 && git clone --depth=1 https://github.com/UltimateHackers/Striker /opt/Striker \
 && git clone --depth=1 https://github.com/v3n0m-Scanner/V3n0M-Scanner /opt/V3n0M-Scanner \
 && git clone --depth=1 https://github.com/D35m0nd142/LFISuite /opt/LFISuite \
 && git clone --depth=1 https://github.com/P0cL4bs/Kadimus /opt/Kadimus \
 && git clone --depth=1 https://github.com/WhitewidowScanner/whitewidow /opt/whitewidow
#   SQL Injection
RUN (apt-get update ;apt-get install --no-install-recommends sqlmap=1.7.3-1 themole=0.3-3.1 -y )
#   Phishing
RUN (apt-get update ;apt-get install --no-install-recommends httrack=3.49.4-1 -y ) \
 && git clone --depth=1 https://github.com/UndeadSec/SocialFish.git /opt/SocialFish \
 && git clone --depth=1 https://github.com/boxug/trape /opt/trape \
 && git clone --depth=1 https://github.com/kgretzky/evilginx /opt/evilginx
#   DOS Tools
RUN (apt-get update ;apt-get install --no-install-recommends t50=5.8.7b-1 ) \
 && curl -o /usr/local/bin/slowloris https://raw.githubusercontent.com/ston3o/dotfiles/master/bin/slowloris.pl \
 && chmod +x /usr/local/bin/slowloris \
 && git clone --depth=1 https://github.com/Souhardya/Warchild /opt/Warchild
#   Kickthemout
RUN git clone --depth=1 https://github.com/k4m4/kickthemout /opt/kickthemout \
 && cd /opt/kickthemout/ \
 && python -m pip install -r requirements.txt \
 && ln -s /opt/kickthemout/kickthemout.py /usr/local/bin/kickthemout \
 && chmod +x kickthemout.py
#   NMAP NSE Script vulscan
RUN curl -s http://www.computec.ch/projekte/vulscan/download/nmap_nse_vulscan-2.0.tar.gz | tar xzvf - -C /usr/share/nmap/scripts/ \
 && wget -P /usr/share/nmap/scripts/ https://raw.githubusercontent.com/cldrn/nmap-nse-scripts/master/scripts/smb-vuln-ms17-010.nse \
 && wget -P /usr/share/nmap/scripts/ https://raw.githubusercontent.com/vulnersCom/nmap-vulners/master/vulners.nse
#   Wireless networks, WEP WPA
RUN (apt-get update ;apt-get install --no-install-recommends wifite=2.6.6-1 wifiphisher mdk3=6.0-8ubuntu1 tshark=4.0.3-1 -y )
RUN git clone --depth=1 https://github.com/kylemcdonald/FreeWifi /opt/FreeWifi \
 && cd /opt/FreeWifi \
 && pip install -r requirements.txt \
 && git clone --depth=1 https://github.com/FluxionNetwork/fluxion /opt/fluxion \
 && git clone --depth=1 https://github.com/chrizator/netattack /opt/netattack \
 && git clone --depth=1 https://github.com/chrizator/netattack2/ /opt/netattack2 \
 && git clone --depth=1 https://github.com/s0lst1c3/eaphammer /opt/eaphammer \
 && git clone --depth=1 https://github.com/P0cL4bs/WiFi-Pumpkin /opt/Wifi-Pumpkin \
 && git clone --depth=1 https://github.com/M1ND-B3ND3R/BoopSuite /opt/BoopSuite
#   Reverse Engineering
RUN (apt-get update ;apt-get install --no-install-recommends apktool=2.7.0+dfsg-4 set -y ) \
 && git clone --depth=1 https://github.com/radare/radare2 /opt/radare2
#   Backdoor / Remote Access Trojan (RAT)
RUN git clone --depth=1 https://github.com/Screetsec/TheFatRat /opt/TheFatRat \
 && echo "*\n*\n*\n*\nmsfconsole\nmsfvenom\nbackdoor-factory\nsearchsploit" > /opt/TheFatRat/config/config.path \
 && chmod +x /opt/TheFatRat/fatrat
RUN git clone --depth=1 https://github.com/jbreed/apkinjector /opt/apkinjector \
 && chmod +x /opt/apkinjector/apkinjector
RUN git clone --depth=1 https://github.com/dana-at-cp/backdoor-apk /opt/backdoor-apk \
 && sed -i -e 's/ZIPALIGN=.*$/ZIPALIGN=\/usr\/bin\/zipalign/g' /opt/backdoor-apk/backdoor-apk/backdoor-apk.sh
RUN git clone --depth=1 https://github.com/r00t-3xp10it/backdoorppt /opt/backdoorppt \
 && sed -i -e 's/BASH_TRANSFORMATION=NO/BASH_TRANSFORMATION=YES/g' /opt/backdoorppt/settings \
 && sed -i -e 's/RESOURCEHACKER_BYPASS=NO/RESOURCEHACKER_BYPASS=YES/g' /opt/backdoorppt/settings
RUN git clone --depth=1 https://github.com/Screetsec/microsploit /opt/microsploit \
 && git clone --depth=1 https://github.com/nccgroup/Winpayloads /opt/Winpayloads \
 && git clone --depth=1 https://github.com/tiagorlampert/CHAOS /opt/CHAOS \
 && git clone --depth=1 https://github.com/vesche/basicRAT /opt/basicRAT \
 && git clone --depth=1 https://github.com/n1nj4sec/pupy /opt/pupy \
 && git clone --depth=1 https://github.com/quasar/QuasarRAT /opt/QuasarRAT
#   Search exploit
RUN (apt-get update ;apt-get install --no-install-recommends exploitdb -y )
RUN git clone --depth=1 https://github.com/vulnersCom/getsploit /opt/getsploit \
 && git clone --depth=1 https://github.com/1N3/findsploit /opt/findsploit \
 && git clone --depth=1 https://github.com/MalwareReverseBrasil/malwaresearch /opt/malwaresearch
#   Post exploitation
RUN git clone --depth=1 https://github.com/nathanlopez/Stitch /opt/Stitch \
 && git clone --depth=1 https://github.com/putterpanda/mimikittenz /opt/mimikittenz \
 && git clone --depth=1 https://github.com/nil0x42/phpsploit /opt/phpsploit \
 && git clone --depth=1 https://github.com/huntergregal/mimipenguin /opt/mimipenguin
#   Privilege escalation
RUN git clone --depth=1 https://github.com/ngalongc/AutoLocalPrivilegeEscalation /opt/AutoLocalPrivilegeEscalation \
 && git clone --depth=1 https://github.com/rasta-mouse/Sherlock /opt/Sherlock \
 && git clone --depth=1 https://github.com/AlessandroZ/BeRoot /opt/BeRoot \
 && git clone --depth=1 https://github.com/NullArray/MIDA-Multitool /opt/MIDA \
 && git clone --depth=1 https://github.com/nilotpalbiswas/Auto-Root-Exploit /opt/Auto-Root-Exploit
#   Forensic
RUN (apt-get update ;apt-get install --no-install-recommends extract=1:1.11-7build1 -y )
RUN git clone --depth=1 https://github.com/volatilityfoundation/volatility /opt/volatility
#   Geolocalisation
RUN git clone --depth=1 https://github.com/maldevel/IPGeoLocation /opt/IPGeoLocation \
 && cd /opt/IPGeoLocation \
 && pip3 install -r requirements.txt \
 && chmod +x /opt/IPGeoLocation/ipgeolocation.py
#   Shellcode
RUN git clone --depth=1 https://github.com/reyammer/shellnoob /opt/shellnoob
#   Wordpress
RUN (apt-get update ;apt-get install --no-install-recommends wpscan -y )
RUN git clone --depth=1 https://github.com/n00py/WPForce /opt/WPForce \
 && git clone --depth=1 https://github.com/m4ll0k/WPSeku /opt/WPSeku
#   Bypass CloudFlare
RUN git clone --depth=1 https://github.com/HatBashBR/HatCloud /opt/HatCloud \
 && git clone --depth=1 https://github.com/m0rtem/CloudFail /opt/CloudFail
#   Steganography
RUN git clone --depth=1 https://github.com/solusipse/spectrology /opt/spectrology
#   Honeypot
RUN git clone --depth=1 https://github.com/droberson/ssh-honeypot /opt/ssh-honeypot \
 && git clone --depth=1 https://github.com/micheloosterhof/cowrie /opt/cowrie
#   Reverse shell
RUN (apt-get update ;apt-get install --no-install-recommends netcat -y )
RUN git clone --depth=1 https://github.com/mthbernardes/rsg /opt/rsg
#   Tunnel
RUN git clone --depth=1 https://github.com/yarrick/iodine /opt/iodine
#   Detect WAF
RUN git clone --depth=1 https://github.com/EnableSecurity/wafw00f /opt/wafw00f \
 && git clone --depth=1 https://github.com/Ekultek/WhatWaf /opt/WhatWaf
#   Remove file metadata
RUN (apt-get update ;apt-get install --no-install-recommends mat=0.13.2-1 -y )
#   AV Evasion / Dropper
RUN git clone --depth=1 https://github.com/jbreed/apkwash /opt/apkwash \
 && git clone --depth=1 https://github.com/D4Vinci/Dr0p1t-Framework /opt/Dr0p1t-Framework
#   Disassembler / Decompiler
RUN git clone --depth=1 https://github.com/intezer/docker-ida /opt/docker-ida \
 && git clone --depth=1 https://github.com/avast-tl/retdec /opt/retdec
#   Fuzzing
RUN git clone --depth=1 https://github.com/joxeankoret/nightmare /opt/nightmare
#   Paquet manipulation
RUN (apt-get update ;apt-get install --no-install-recommends hping3=3.a2.ds2-10 scapy -y )
#   C&C
RUN git clone --depth=1 https://github.com/Ne0nd0g/merlin /opt/merlin \
 && git clone --depth=1 https://github.com/zerosum0x0/koadic /opt/koadic
#   botnet
RUN git clone --depth=1 https://github.com/sweetsoftware/Ares /opt/Ares \
 && git clone --depth=1 https://github.com/colental/byob /opt/byob
#   Keylogger
RUN git clone --depth=1 https://github.com/tiagorlampert/sAINT /opt/sAINT
#  ########
#   CLEAN #
#  ########
RUN apt-get autoremove -y \
 && rm -rf /tmp/* \
 && rm -rf /var/lib/{apt,dpkg,cache,log,tmp}/*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
