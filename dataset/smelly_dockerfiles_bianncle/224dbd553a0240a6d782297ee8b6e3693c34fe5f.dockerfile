# ==============================================================================
#  Copyright (c) 2016-present Allan CORNET (Nelson)
# ==============================================================================
#  This file is part of the Nelson.
# =============================================================================
#  LICENCE_BLOCK_BEGIN
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public
#  License as published by the Free Software Foundation; either
#  version 2.1 of the License, or (at your option) any later version.
#
#  Alternatively, you can redistribute it and/or
#  modify it under the terms of the GNU General Public License as
#  published by the Free Software Foundation; either version 2 of
#  the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with this program. If not, see <http://www.gnu.org/licenses/>.
#  LICENCE_BLOCK_END
# ==============================================================================
FROM debian:buster
MAINTAINER Allan CORNET "nelson.numerical.computation@gmail.com"
ARG BRANCH_NAME
RUN echo "Nelson's branch: ${BRANCH_NAME}"
RUN apt-get update
RUN apt-get install build-essential -y
RUN apt-get install autotools-dev -y
RUN apt-get install libtool -y
RUN apt-get install automake -y
RUN apt-get install xvfb -y
RUN apt-get install git -y
RUN apt-get install libboost-all-dev -y
RUN apt-get install libopenmpi-dev -y
RUN apt-get install openmpi-bin -y
RUN apt-get install gettext -y
RUN apt-get install pkg-config -y
RUN apt-get install cmake -y
RUN apt-get install libffi-dev -y
RUN apt-get install libicu-dev -y
RUN apt-get install libxml2-dev -y
RUN apt-get install liblapack-dev -y
RUN apt-get install liblapacke-dev -y
RUN apt-get install fftw3 -y
RUN apt-get install fftw3-dev -y
RUN apt-get install libasound-dev -y
RUN apt-get install portaudio19-dev -y
RUN apt-get install libsndfile1-dev -y
RUN apt-get install libtag1-dev -y
RUN apt-get install alsa-utils -y
RUN apt-get install qtbase5-dev -y
RUN apt-get install qtdeclarative5-dev -y
RUN apt-get install libqt5webkit5-dev -y
RUN apt-get install libsqlite3-dev -y
RUN apt-get install qt5-default qttools5-dev-tools -y
RUN apt-get install libqt5opengl5-dev -y
RUN apt-get install qtbase5-private-dev -y
RUN apt-get install qtdeclarative5-dev -y
RUN apt-get install libhdf5-dev -y
RUN apt-get install hdf5-tools -y
RUN apt-get install libmatio-dev -y
RUN apt-get install libslicot0 -y
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
