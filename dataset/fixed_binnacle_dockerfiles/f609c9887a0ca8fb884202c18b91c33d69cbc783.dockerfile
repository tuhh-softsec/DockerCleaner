#
#   Dockerfile for PostgreSQL 10 server
#
#   Copyright (c) 2018-2019 Qualcomm Technologies, Inc.
#   SPDX-License-Identifier: BSD-4-Clause-Clear
#
#   Copyright (c) 2018-2019 Qualcomm Technologies, Inc.
#
#   All rights reserved.
#
#   Redistribution and use in source and binary forms, with or without modification, are permitted (subject to the
#   limitations in the disclaimer below) provided that the following conditions are met:
#
#      - Redistributions of source code must retain the above copyright notice, this list of conditions and the following
#        disclaimer.
#      - Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the
#        following disclaimer in the documentation and/or other materials provided with the distribution.
#      - All advertising materials mentioning features or use of this software, or any deployment of this software,
#        or documentation accompanying any distribution of this software, must display the trademark/logo as per the
#        details provided here: https://www.qualcomm.com/documents/dirbs-logo-and-brand-guidelines
#      - Neither the name of Qualcomm Technologies, Inc. nor the names of its contributors may be used to endorse or
#        promote products derived from this software without specific prior written permission.
#
#
#
#   SPDX-License-Identifier: ZLIB-ACKNOWLEDGEMENT
#
#   Copyright (c) 2018-2019 Qualcomm Technologies, Inc.
#
#   This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable
#   for any damages arising from the use of this software. Permission is granted to anyone to use this software for any
#   purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following
#   restrictions:
#
#      - The origin of this software must not be misrepresented; you must not claim that you wrote the original software.
#        If you use this software in a product, an acknowledgment is required by displaying the trademark/logo as per the
#        details provided here: https://www.qualcomm.com/documents/dirbs-logo-and-brand-guidelines
#      - Altered source versions must be plainly marked as such, and must not be misrepresented as being the original
#        software.
#      - This notice may not be removed or altered from any source distribution.
#
#   NO EXPRESS OR IMPLIED LICENSES TO ANY PARTY'S PATENT RIGHTS ARE GRANTED BY THIS LICENSE. THIS SOFTWARE IS PROVIDED BY
#   THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
#   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
#   COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
#   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
#   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#   POSSIBILITY OF SUCH DAMAGE.
#
FROM ubuntu:16.04
#   Set environment (set proper unicode locale, hush debconfig, etc.
#   Set PATH so that subsequent pip3 commands install into virtualenv.
#   activate command does not work within Docker for some reason
ENV DEBIAN_FRONTEND="noninteractive" \
    LC_ALL="C.UTF-8" \
    LANG="C.UTF-8" \
    PATH="/usr/lib/postgresql/10/bin:$PATH" \
    GOSU_VERSION="1.10" \
    HLL_VERSION="2.10.2"
#
#   - Set default shell to bash,
#   - Update package lists
#   - Install APT depdendencies
#
RUN set -x \
 && unlink /bin/sh ; ln -s bash /bin/sh \
 && apt-get update -q --fix-missing \
 && apt-get install --no-install-recommends locales=2.23-0ubuntu11.3 wget=1.17.1-1ubuntu1.5 ca-certificates=20210119~16.04.1 unzip=6.0-20ubuntu1.1 build-essential=12.1ubuntu2 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#
#   Set default locale
#
RUN update-locale LC_ALL=C.UTF-8 LANG=C.UTF-8
#   Add PostgreSQL user and group
RUN set -x \
 && groupadd -r postgres --gid=999 \
 && useradd -r -g postgres --uid=999 postgres
#   Install Postgres 10, but don't create a cluster
RUN set -x \
 && echo 'deb http://apt.postgresql.org/pub/repos/apt/ xenial-pgdg main' > /etc/apt/sources.list.d/pgdg.list \
 && wget -q -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
 && apt-get update -q --fix-missing \
 && apt-get install --no-install-recommends postgresql-common=173ubuntu0.3 pgtop=3.7.0-2build1 postgresql-server-dev-10 -q -y \
 && sed -ri 's/#(create_main_cluster) .*$/\1 = false/' /etc/postgresql-common/createcluster.conf \
 && apt-get install --no-install-recommends postgresql-10 postgresql-contrib-10 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Install gosu
RUN set -x \
 && wget -q -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;)" \
 && chmod +x /usr/local/bin/gosu \
 && gosu nobody true
#   install hll
RUN set -x \
 && wget -q -O hll.zip https://github.com/citusdata/postgresql-hll/archive/v$HLL_VERSION.zip \
 && unzip hll.zip \
 && cd postgresql-hll-$HLL_VERSION \
 && make install
#   Create /var/run/postgresql folder
RUN set -x \
 && mkdir -p /var/run/postgresql \
 && chown -R postgres /var/run/postgresql
#   Copy config to the data directory
COPY docker/prd/postgresql10/*.conf /etc/db_config/
#   Copy entrypoint
COPY docker/prd/postgresql10/entrypoint.sh /
#   Make sure permissions are set properly on entrypoint
RUN set -x \
 && chmod a+x /entrypoint.sh
#   Expose port
EXPOSE 5432/tcp
#   Set entrypoint
ENTRYPOINT ["/entrypoint.sh"]
#   Set default command (argument to entrypoint.sh)
CMD ["postgres"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
