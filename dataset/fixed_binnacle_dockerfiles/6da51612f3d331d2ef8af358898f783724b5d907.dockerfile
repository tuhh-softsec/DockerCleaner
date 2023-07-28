FROM ubuntu:16.04
RUN apt-get update -y \
 && apt-get install --no-install-recommends autoconf=2.69-9 automake=1:1.15-4ubuntu1 avrdude=6.2-5 avr-libc=1:1.8.0+Atmel3.5.0-1 bash-completion=1:2.1-4.2ubuntu1.1 binutils-avr=2.25+Atmel3.5.0-2 bison=2:3.0.4.dfsg-1 bossa-cli=1.3~20120408-5build1 bpython=0.15-2 bpython3=0.15-2 ckermit=302-5build1 cloc=1.60-1.1 cppcheck=1.72-1 doxygen=1.8.11-1ubuntu0.1 emacs24=24.5+1-6ubuntu1.1 flex=2.6.0-11 g++=4:5.3.1-1ubuntu1 gawk=1:4.1.3+dfsg-0.1 gcc=4:5.3.1-1ubuntu1 gcc-arm-none-eabi=15:4.9.3+svn231177-1 gcc-avr=1:4.9.2+Atmel3.5.0-1 gdb-avr=7.7-2build1 genext2fs=1.4.1-4build1 git=1:2.7.4-0ubuntu1.10 gitk=1:2.7.4-0ubuntu1.10 gperf=3.0.4-2 help2man=1.47.3 lcov=1.12-2 libexpat-dev libtool=2.4.6-0.1 libtool-bin=2.4.6-0.1 make=4.1-6 ncurses-dev pmccabe=2.6 python=2.7.12-1~16.04 python3=3.5.1-3 python3-pip=8.1.1-2ubuntu0.6 python-pip=8.1.1-2ubuntu0.6 python-lzma=0.5.3-3 python-pyelftools=0.23-2 sed=4.2.2-7 texinfo=6.1.0.dfsg.1-5 u-boot-tools=2016.01+dfsg1-2ubuntu5 unrar=1:5.3.2-1+deb9u1build0.16.04.1 unzip=6.0-20ubuntu1.1 valgrind=1:3.11.0-1ubuntu4.2 wget=1.17.1-1ubuntu1.5 -y
RUN pip install breathe==4.35.0 pyserial==3.5 readchar==4.0.5 sphinx==6.1.3 sphinx_rtd_theme==1.2.0 xpect==3.1.1
RUN cd /opt \
 && wget https://github.com/eerimoq/simba-releases/raw/master/arduino/esp32/tools/xtensa-esp32-elf-linux$( getconf LONG_BIT ;)-1.22.0-59.tar.gz \
 && tar xf xtensa-esp32-elf-linux$( getconf LONG_BIT ;)-1.22.0-59.tar.gz \
 && rm xtensa-esp32-elf-linux$( getconf LONG_BIT ;)-1.22.0-59.tar.gz
RUN mkdir tmp2
RUN groupadd -r test \
 && useradd --no-log-init -r -g test test
RUN chown -R test tmp2 \
 && chmod -R u+rX tmp2
USER test
RUN cd tmp2 \
 && git clone --recursive https://github.com/pfalcon/esp-open-sdk \
 && cd esp-open-sdk \
 && make
USER root
RUN mv /tmp2/esp-open-sdk/xtensa-lx106-elf /opt \
 && rm -rf tmp2
RUN cd /opt \
 && wget https://releases.linaro.org/components/toolchain/binaries/7.2-2017.11/aarch64-elf/gcc-linaro-7.2.1-2017.11-x86_64_aarch64-elf.tar.xz \
 && tar xf gcc-linaro-7.2.1-2017.11-x86_64_aarch64-elf.tar.xz \
 && rm gcc-linaro-7.2.1-2017.11-x86_64_aarch64-elf.tar.xz
RUN cd /opt \
 && wget https://releases.linaro.org/components/toolchain/binaries/7.2-2017.11/aarch64-linux-gnu/gcc-linaro-7.2.1-2017.11-x86_64_aarch64-linux-gnu.tar.xz \
 && tar xf gcc-linaro-7.2.1-2017.11-x86_64_aarch64-linux-gnu.tar.xz \
 && rm gcc-linaro-7.2.1-2017.11-x86_64_aarch64-linux-gnu.tar.xz
ENV SIMBA_DOCKER="yes"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
