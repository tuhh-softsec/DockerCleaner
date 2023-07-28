#
#   Base Dockerfile for DIRBS
#
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
    PATH="/home/dirbs/dirbs-venv/bin:/opt/validator/bin:$PATH" \
    GOSU_VERSION="1.10"
#
#   - Set default shell to bash,
#   - Update package lists
#   - Install APT depdendencies
#
RUN set -x \
 && unlink /bin/sh ; ln -s bash /bin/sh \
 && apt-get update -q --fix-missing \
 && apt-get install --no-install-recommends locales=2.23-0ubuntu11.3 apt-utils=1.2.35 git=1:2.7.4-0ubuntu1.10 libpq-dev=9.5.25-0ubuntu0.16.04.1 python3-pip=8.1.1-2ubuntu0.6 python3-venv=3.5.1-3 zip=3.0-11 unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 default-jre-headless=2:1.8-56ubuntu2 build-essential=12.1ubuntu2 python3-dev=3.5.1-3 vim=2:7.4.1689-3ubuntu1.5 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#
#   Set default locale
#
RUN update-locale LC_ALL=C.UTF-8 LANG=C.UTF-8
#   Install CSV validator
RUN set -x \
 && wget -q http://search.maven.org/remotecontent?filepath=uk/gov/nationalarchives/csv-validator-cmd/1.2-RC2/csv-validator-cmd-1.2-RC2-application.zip -O csv-validator-cmd-1.2-RC2-application.zip -nv \
 && unzip csv-validator-cmd-1.2-RC2-application.zip \
 && mv csv-validator-cmd-1.2-RC2 /opt/validator
#   Install gosu
RUN set -x \
 && wget -q -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;)" \
 && chmod +x /usr/local/bin/gosu \
 && gosu nobody true
#   Create non-root dirbs user
RUN set -x \
 && groupadd -g 9001 dirbs \
 && useradd -m -d /home/dirbs -s /bin/bash -u 9001 -g 9001 dirbs
#
#   - Create DIRBS virtualenv
#   - Upgrade pip and install wheel
#   - Fix permissions, as pip3 installs are being done as root here
#   - Make sure that a .dirbsenv file is always sourced into the environment if present. This file
#     contains environment variables like DIRBS_DB_HOSTNAME if they were provided when the processing container
#     was started
#
RUN set -x \
 && echo 'test -z "$VIRTUAL_ENV" \
 && source /home/dirbs/dirbs-venv/bin/activate' >> /home/dirbs/.bashrc \
 && pyvenv /home/dirbs/dirbs-venv \
 && pip3 install --upgrade pip setuptools \
 && pip3 install wheel \
 && chown -R dirbs.dirbs /home/dirbs/.bashrc /home/dirbs/dirbs-venv \
 && echo 'if [ -f /home/dirbs/.dirbsenv ]; then source /home/dirbs/.dirbsenv; fi' >> /home/dirbs/dirbs-venv/bin/activate
#   Create log volume dir and make sure it has the correct permissions
RUN set -x \
 && mkdir -p /var/log/dirbs \
 && chown -R dirbs.dirbs /var/log/dirbs
#   Mark log volume
VOLUME ["/var/log/dirbs"]
#   Override in child container
CMD ["gosu", "dirbs", "/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
