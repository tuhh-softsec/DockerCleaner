#
#   ***** BEGIN LICENSE BLOCK *****
#   
#   Copyright (C) 2009-2019 Olof Hagsand and Benny Holmgren
#
#   This file is part of CLIXON
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#   Alternatively, the contents of this file may be used under the terms of
#   the GNU General Public License Version 3 or later (the "GPL"),
#   in which case the provisions of the GPL are applicable instead
#   of those above. If you wish to allow use of your version of this file only
#   under the terms of the GPL, and not to allow others to
#   use your version of this file under the terms of Apache License version 2, 
#   indicate your decision by deleting the provisions above and replace them with
#   the notice and other provisions required by the GPL. If you do not delete
#   the provisions above, a recipient may use your version of this file under
#   the terms of any one of the Apache License version 2 or the GPL.
#
#   ***** END LICENSE BLOCK *****
#
FROM alpine
MAINTAINER Olof Hagsand <olof@hagsand.se>
#   For clixon and cligen
RUN apk add git=2.38.4-r1 make=4.3-r1 build-base=0.5-r3 gcc=12.2.1_git20220924-r4 flex=2.6.4-r3 bison=3.8.2-r0 fcgi-dev=2.4.2-r2 curl-dev=7.88.1-r1 --update
#   Create a directory to hold source-code, dependencies etc
RUN mkdir /clixon
RUN mkdir /clixon/build
WORKDIR /clixon
#   Clone cligen
RUN git clone https://github.com/olofhagsand/cligen.git
#   Build cligen
WORKDIR /clixon/cligen
RUN ./configure --prefix=/clixon/build
RUN make
RUN make install
#   Copy Clixon from local dir
RUN mkdir /clixon/clixon
WORKDIR /clixon/clixon
COPY clixon .
#   Need to add www user manually
RUN adduser -D -H www-data
#   nginx adds group www-data
RUN apk add nginx=1.22.1-r0 --update
#   Configure, build and install clixon
RUN ./configure --prefix=/clixon/build --with-cligen=/clixon/build --with-wwwuser=www-data
RUN make
RUN make install
RUN make install-include
#   Install utils
WORKDIR /clixon/clixon/util
RUN make
RUN make install
#   Build and install the clixon example
WORKDIR /clixon/clixon/example/main
RUN make
RUN make install
RUN install example.xml /clixon/build/etc/clixon.xml
#   Copy tests
WORKDIR /clixon/clixon/test
RUN install -d /clixon/build/bin/test
RUN install *.sh /clixon/build/bin/test
#   Copy startscript
WORKDIR /clixon
COPY startsystem.sh startsystem.sh
RUN install startsystem.sh /clixon/build/bin/
#
#   Stage 2
#
FROM alpine
MAINTAINER Olof Hagsand <olof@hagsand.se>
#   For clixon and cligen
RUN apk add flex=2.6.4-r3 bison=3.8.2-r0 fcgi-dev=2.4.2-r2 --update
#   need to add www user manually
RUN adduser -D -H www-data
#   nginx adds group www-data
RUN apk add nginx=1.22.1-r0 --update
#   Test-specific (for test scripts)
RUN apk add sudo=1.9.12_p2-r1 curl=7.88.1-r1 procps=3.3.17-r2 grep=3.8-r1 make=4.3-r1 bash=5.2.15-r0 --update
#   Expose nginx port for restconf
EXPOSE 80/tcp
#   Create clicon group
RUN addgroup clicon
RUN adduser nginx clicon
RUN adduser www-data clicon
COPY --from=0 /clixon/build/ /usr/local/
COPY --from=0 /www-data /www-data
#   Manually created
RUN chown www-data /www-data
RUN chgrp www-data /www-data
#   Log to stderr.
CMD /usr/local/bin/startsystem.sh
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
