#  ==============================================================================
#   Copyright (c) 2016-present Allan CORNET (Nelson)
#  ==============================================================================
#   This file is part of the Nelson.
#  =============================================================================
#   LICENCE_BLOCK_BEGIN
#   This program is free software; you can redistribute it and/or
#   modify it under the terms of the GNU Lesser General Public
#   License as published by the Free Software Foundation; either
#   version 2.1 of the License, or (at your option) any later version.
#
#   Alternatively, you can redistribute it and/or
#   modify it under the terms of the GNU General Public License as
#   published by the Free Software Foundation; either version 2 of
#   the License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU Lesser General Public License for more details.
#
#   You should have received a copy of the GNU Lesser General Public
#   License along with this program. If not, see <http://www.gnu.org/licenses/>.
#   LICENCE_BLOCK_END
#  ==============================================================================
FROM debian:buster
MAINTAINER Allan CORNET "nelson.numerical.computation@gmail.com"
ARG BRANCH_NAME
RUN echo "Nelson's branch: ${BRANCH_NAME}"
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=12.6 -y )
RUN (apt-get update ;apt-get install --no-install-recommends autotools-dev=20180224.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libtool=2.4.6-9 -y )
RUN (apt-get update ;apt-get install --no-install-recommends automake=1:1.16.1-4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends xvfb=2:1.20.4-1+deb10u9 -y )
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.20.1-2+deb10u8 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libboost-all-dev=1.67.0.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libopenmpi-dev=3.1.3-11 -y )
RUN (apt-get update ;apt-get install --no-install-recommends openmpi-bin=3.1.3-11 -y )
RUN (apt-get update ;apt-get install --no-install-recommends gettext=0.19.8.1-9 -y )
RUN (apt-get update ;apt-get install --no-install-recommends pkg-config=0.29-6 -y )
RUN (apt-get update ;apt-get install --no-install-recommends cmake=3.13.4-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libffi-dev=3.2.1-9 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libicu-dev=63.1-6+deb10u3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libxml2-dev=2.9.4+dfsg1-7+deb10u5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends liblapack-dev=3.8.0-2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends liblapacke-dev=3.8.0-2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends fftw3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends fftw3-dev -y )
RUN (apt-get update ;apt-get install --no-install-recommends libasound-dev -y )
RUN (apt-get update ;apt-get install --no-install-recommends portaudio19-dev=19.6.0-1+deb10u1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libsndfile1-dev=1.0.28-6+deb10u2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libtag1-dev=1.11.1+dfsg.1-0.3+deb10u1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends alsa-utils=1.1.8-2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends qtbase5-dev=5.11.3+dfsg1-1+deb10u5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends qtdeclarative5-dev=5.11.3-4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libqt5webkit5-dev=5.212.0~alpha2-21 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libsqlite3-dev=3.27.2-3+deb10u2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends qt5-default=5.11.3+dfsg1-1+deb10u5 qttools5-dev-tools=5.11.3-4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libqt5opengl5-dev=5.11.3+dfsg1-1+deb10u5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends qtbase5-private-dev=5.11.3+dfsg1-1+deb10u5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends qtdeclarative5-dev=5.11.3-4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libhdf5-dev=1.10.4+repack-10 -y )
RUN (apt-get update ;apt-get install --no-install-recommends hdf5-tools=1.10.4+repack-10 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libmatio-dev=1.5.13-3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libslicot0=5.0+20101122-4 -y )
RUN rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/live-clones/hdf5.git /tmp/hdf5_1_10_5
RUN cd /tmp/hdf5_1_10_5 \
 && git checkout hdf5-1_10_5 \
 && ./configure --quiet --enable-shared --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-memory-alloc-sanity-check --disable-instrument --disable-parallel --disable-trace --disable-asserts --with-pic --with-default-api-version=v110 CFLAGS="-w" \
 && make install -C src
RUN git clone https://github.com/tbeu/matio /tmp/matio
RUN cd /tmp/matio \
 && git checkout v1.5.15 \
 && cd /tmp/matio \
 && ./autogen.sh \
 && ./configure --enable-shared --enable-mat73=yes --enable-extended-sparse=no --with-pic --with-hdf5=/tmp/hdf5_1_10_5/hdf5 \
 && make \
 && make install
RUN git clone https://github.com/eigenteam/eigen-git-mirror /tmp/eigen
RUN mkdir /tmp/eigen-build \
 && cd /tmp/eigen \
 && git checkout 3.3.7 \
 && cd - \
 && cd /tmp/eigen-build \
 && cmake . /tmp/eigen \
 && make -j4 \
 && make install
RUN git clone https://github.com/Nelson-numerical-software/nelson.git
WORKDIR "/nelson"
RUN cd "/nelson" \
 && git checkout ${BRANCH_NAME}
RUN mkdir /home/nelsonuser
RUN groupadd -g 999 nelsonuser \
 && useradd -r -u 999 -g nelsonuser nelsonuser
RUN chown -R nelsonuser:nelsonuser /home/nelsonuser
RUN chown -R nelsonuser:nelsonuser /nelson
USER nelsonuser
ENV AUDIODEV="null"
RUN cmake -G "Unix Makefiles" .
RUN make -j4
RUN make buildhelp
RUN make tests_minimal
RUN make package
RUN make tests_all_no_display
ENTRYPOINT ["/nelson/bin/linux64/nelson-cli"]
# Please add your HEALTHCHECK here!!!
