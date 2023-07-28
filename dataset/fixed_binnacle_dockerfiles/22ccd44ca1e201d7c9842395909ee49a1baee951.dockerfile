FROM tatsushid/tinycore:7.2-x86_64
#   Instructions are run with 'tc' user
#   http://bugs.python.org/issue19846
#   > At the moment, setting "LANG=C" on a Linux system *fundamentally breaks Python 3*, and that's not OK.
ENV LANG="C.UTF-8" \
    LC_ALL="C"
RUN tce-load -wic gnupg curl \
 && rm -rf /tmp/tce/optional/*
#   gpg: key F73C700D: public key "Larry Hastings <larry@hastings.org>" imported
RUN gpg2 --keyserver pool.sks-keyservers.net --recv-keys 97FC712E4C024BBEA48A61ED3A5CA953F73C700D
ENV PYTHON_VERSION="3.5.2"
#   if this is called "PIP_VERSION", pip explodes with "ValueError: invalid truth value '<VERSION>'"
ENV PYTHON_PIP_VERSION="9.0.1"
RUN tce-load -wic bzip2-dev flex file gcc make linux-4.2.1_api_headers glibc_add_lib glibc_base-dev openssl-dev gdbm-dev ncurses-dev readline-dev sqlite3-dev liblzma-dev zlib_base-dev tk-dev libX11-dev libXss xorg-proto \
 && cd /tmp \
 && curl -SL "https://www.python.org/ftp/python/$PYTHON_VERSION/Python-$PYTHON_VERSION.tar.xz" -o python.tar.xz \
 && curl -SL "https://www.python.org/ftp/python/$PYTHON_VERSION/Python-$PYTHON_VERSION.tar.xz.asc" -o python.tar.xz.asc \
 && gpg2 --verify python.tar.xz.asc \
 && rm python.tar.xz.asc \
 && tar -xJf python.tar.xz \
 && rm python.tar.xz \
 && cd "/tmp/Python-$PYTHON_VERSION" \
 && ./configure --enable-shared \
 && make \
 && mkdir tmp_install \
 && make install DESTDIR=tmp_install \
 && for F in `find tmp_install | xargs file | grep "executable" | grep ELF | grep "not stripped" | cut -f 1 -d : `; do [ -f $F ] \
 && strip --strip-unneeded $F ; done \
 && for F in `find tmp_install | xargs file | grep "shared object" | grep ELF | grep "not stripped" | cut -f 1 -d : `; do [ -f $F ] \
 && if [ ! -w $F ] ; then chmod u+w $F \
 && strip -g $F \
 && chmod u-w $F ; else strip -g $F ; fi ; done \
 && for F in `find tmp_install | xargs file | grep "current ar archive" | cut -f 1 -d : `; do [ -f $F ] \
 && strip -g $F ; done \
 && find tmp_install
#   Instructions after here are run with 'root' user
USER root
RUN cd /usr/local/bin \
 && ln -s easy_install-3.5 easy_install \
 && ln -s idel3 idle \
 && ln -s pydoc3 pydoc \
 && ln -s python3 python \
 && ln -s python3-config python-config
CMD ["python3"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
