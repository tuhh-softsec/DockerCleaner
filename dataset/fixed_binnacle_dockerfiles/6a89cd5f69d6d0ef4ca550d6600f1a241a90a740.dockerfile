FROM fedora
MAINTAINER Buttetsu Batou <doubledense@gmail.com>
#   Install dependencies and audio tools
RUN dnf groupinstall -y "C Development Tools and Libraries"
RUN dnf install -y git zsh wget man sudo
RUN dnf install -y libsndfile-devel libsamplerate-devel liblo-devel jack-audio-connection-kit-devel jack-audio-connection-kit-example-clients alsa-lib-devel xz htop grep procps-ng yasm screen supervisor openssh-server
RUN dnf install -y cabal-install ghc-Cabal-devel
#   Install editor
RUN dnf -y install emacs-nox emacs-haskell-mode
#   Build Dirt synth
WORKDIR /repos
RUN git clone --recursive https://github.com/tidalcycles/Dirt.git
WORKDIR Dirt
RUN make
#   Build & Install libmp3lame
WORKDIR /repos
RUN git clone https://github.com/rbrito/lame.git
WORKDIR lame
RUN ./configure --prefix=/usr
RUN make install
WORKDIR /repos
RUN rm -fr lame
#   Build & Install ffmpeg, ffserver
WORKDIR /repos
RUN git clone git://source.ffmpeg.org/ffmpeg.git ffmpeg
WORKDIR ffmpeg
RUN ./configure --enable-indev=jack --enable-libmp3lame --enable-nonfree --prefix=/usr
RUN make install
WORKDIR /repos
RUN rm -fr ffmpeg
#   Install Tidebox supervisord config
COPY configs/tidebox.ini /etc/supervisord.d/tidebox.ini
#   Initialize and configure sshd
RUN ssh-keygen -b 1024 -t rsa -f /etc/ssh/ssh_host_key
RUN ssh-keygen -b 1024 -t rsa -f /etc/ssh/ssh_host_rsa_key
RUN ssh-keygen -b 1024 -t dsa -f /etc/ssh/ssh_host_dsa_key
RUN sed -i 's/UsePAM\syes/UsePAM no/' /etc/ssh/sshd_config
#   Expose sshd service
EXPOSE 22/tcp
#   Expose ffserver streaming service
EXPOSE 8090/tcp
#   Pull Tidal Emacs binding
RUN mkdir /repos/tidal
WORKDIR /repos
WORKDIR tidal
RUN wget https://raw.github.com/yaxu/Tidal/master/tidal.el
#   Create and configure Tidal user
RUN useradd tidal -s /bin/zsh
RUN echo 'tidal:livecoding' | chpasswd
RUN echo "tidal ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
USER tidal
ENV HOME="/home/tidal"
WORKDIR /home/tidal
RUN ln -s /repos /home/tidal/repos
RUN ln -s /work /home/tidal/work
#   Install Tidal
RUN cabal update
RUN cabal install tidal
#   Install Oh-My-Zsh
RUN sh -c "$( curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh ;)"
#   Disable Zsh automatic window titling
RUN sed -i 's/# DISABLE_AUTO_TITLE="true"/DISABLE_AUTO_TITLE="true"/g' /home/tidal/.zshrc
#   Install default configurations
COPY configs/emacsrc /home/tidal/.emacs
COPY configs/screenrc /home/tidal/.screenrc
COPY configs/ffserver.conf /home/tidal/ffserver.conf
#   Install default Tidal files
COPY tidal/init.tidal /home/tidal/init.tidal
COPY tidal/hello.tidal /home/tidal/hello.tidal
#   Prepare scratch workspace for version control
RUN sudo mkdir /work
RUN sudo chown -R tidal:tidal /work
WORKDIR /work
RUN mkdir /home/tidal/.ssh
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /home/tidal/.ssh/id_rsa https://raw.githubusercontent.com/DoubleDensity/scratchpool/master/id_rsa-scratchpool
RUN sudo chmod 600 /home/tidal/.ssh/id_rsa
RUN sudo chown tidal.tidal /home/tidal/.ssh/id_rsa
COPY configs/sshconfig /home/tidal/.ssh/config
RUN sudo chmod 600 /home/tidal/.ssh/config
RUN sudo chown tidal.tidal /home/tidal/.ssh/config
RUN ssh-keyscan -H github.com >> ~/.ssh/known_hosts
RUN git clone git@github.com:DoubleDensity/scratchpool.git
WORKDIR /work/scratchpool
RUN git config user.name "Tidebox User"
RUN git config user.email "tidal@jankycloud.com"
#   Set Tidal shell to Screen
USER root
RUN echo "/usr/bin/screen" >> /etc/shells
RUN usermod -s /usr/bin/screen tidal
RUN chown -R tidal.tidal /home/tidal/*.tidal
CMD ["/usr/bin/supervisord"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
