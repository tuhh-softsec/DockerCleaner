#  Copyright 2017 Intel Corporation
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#  ------------------------------------------------------------------------------
#  Description:
#    Build Private Data Objects.
#
#   Configuration (build) paramaters
#   - proxy configuration: 			https_proxy http_proxy ftp_proxy  (default: undefined)
#   - base image with pdo dev environ: 		PDO_DEV_IMAGE (default: pdo-dev)
#     (presumably built from Dockerfile.pde-dev)
#   - sgx-mode:					SGX_MODE (default: SIM)
#   - pdo repo to use:				PDO_REPO_URL  (default: https://github.com/hyperledger-labs/private-data-objects.git)
#   - pdo repo branch to use:			PDO_REPO_BRANCH (default: master)
#   - build in debug mode:			PDO_DEBUG_BUILD (default: 0)
#  Build:
#    $ docker build -f docker/Dockerfile.pdo-build -t pdo-build docker
#    if behind a proxy, you might want to add also below options
#    --build-arg https_proxy=$https_proxy --build-arg http_proxy=$http_proxy --build-arg ftp_proxy=$ftp_proxy
#    if you want to build with different version than from pdo-dev, add a build arg PDO_DEV_IMAGE e.g., --build-arg PDO_DEV_IMAGE=pdo-build-xenial
#    similarly, add --build-arg for any of the other above-listed configuration parameters
#    if you want to build with the source locally commented, then use root-directory of
#    source tree as context directory and add '--build-arg PDO_REPO_URL=file:///tmp/build-src/.git', e.g.,
#       docker build -f docker/Dockerfile.pdo-dev -t pdo-build --build-arg PDO_REPO_URL=file:///tmp/build-src/.git .
#
#  Run:
#    $ cd <directory where you checked out private-data-objects>
#    $ docker run -it pdo-build
#    Notes:
#    - if built with SGX_MODE=HW, then you will have to run image on hw with SGX support and
#      add options '--device=/dev/isgx -v /var/run/aesmd:/var/run/aesmd ')
#      Note: your host SGX PSW runtime should be at a similar level than the one in the container
#      or the PSW/aesmd might cause enclave launch problems
#    - if behind a proxy, you might want to add also below options
#      --env https_proxy=$https_proxy --env http_proxy=$http_proxy --env ftp_proxy=$ftp_proxy
#    - Regardless of SGX_MODE, we build with the default fake SGX values and some
#      default PDO_LEDGER_URL (http://rest-api:8008). If these are different at runtime, e.g.,
#      because the ledger changes and/or you run in SGX HW mode and your sgx keys are at a
#      different place and not mapped via docker volumes to the default location
#      '/project/pdo/src/private-data-objects/build/keys/sgx_mode_${SGX_MODE,,}'
#      you will have to
#      - PDO_SGX_KEY_ROOT env var pointing to the directory with the actual files and/or
#        PDO_LEDGER_URL properly configured ..
#      - unset PDO_SPID PDO_SPID_API_KEY
#      - call 'source /project/pdo/src/private-data-objects/build/common-config.sh'
#      - run 'make -C /project/pdo/src/private-data-objects/build conf'
#    - if you want to debug with gdb and alike, you also might want to add options
#      '--security-opt seccomp=unconfined --security-opt apparmor=unconfined --cap-add=SYS_PTRACE '
#    - for develooping based on source in host you might map source into container with an option
#      like -v $(pwd):/project/pdo/src/private-data-objects/
#
ARG PDO_DEV_IMAGE=pdo-dev
#  Get source of PDO and Sawtooth for the corresponding protobufs
#  to allow using local development branch we copy whatever docker directory is passed
#  (and so would contain .git if we call it as docker build . -f docker/.... which then
#  can be used via PDO_REPO_BRANCH build-arg) but also do that via multi-stage so we don't load
#  the whole stuff into the image itself.
FROM ${PDO_DEV_IMAGE} AS source-extractor
ARG PDO_DEV_IMAGE=pdo-dev
#  for bizare docker reason, we have to redefine it here .#..
ARG PDO_REPO_URL=https://github.com/hyperledger-labs/private-data-objects.git
ARG PDO_REPO_BRANCH=master
RUN mkdir /tmp/build-src
COPY . /tmp/build-src
WORKDIR /project/pdo/
RUN mkdir src \
 && cd src \
 && git clone --single-branch --branch ${PDO_REPO_BRANCH} ${PDO_REPO_URL} private-data-objects
#  Build ..
FROM ${PDO_DEV_IMAGE}
COPY --from=source-extractor /project/pdo /project/pdo
ARG SGX_MODE=SIM
ENV SGX_MODE="${SGX_MODE}"
ARG PDO_DEBUG_BUILD=0
ENV PDO_DEBUG_BUILD="${PDO_DEBUG_BUILD}"
RUN . /etc/profile.d/pdo.sh \
 && echo "export SGX_MODE=${SGX_MODE}" >> /etc/profile.d/pdo.sh \
 && export PDO_LEDGER_URL=http://rest-api:8008 \
 && `/project/pdo/src/private-data-objects/build/common-config.sh -e ` \
 && env | grep PDO | egrep -v 'PDO_ENCLAVE_CODE_SIGN_PEM|PDO_INSTALL_ROOT|PDO_HOME' | sed 's/^PDO/export PDO/g' >> /etc/profile.d/pdo.sh \
 && make -C /project/pdo/src/private-data-objects/build/ NO_SGX_RUN_DURING_BUILD=true \
 && echo '. /project/pdo/build/bin/activate' >> /etc/profile.d/pdo.sh \
 && . /project/pdo/build/bin/activate \
 && cd /project/pdo/src/private-data-objects \
 && sawtooth/bin/build_sawtooth_proto \
 && cd python \
 && python3 setup.py build_ext \
 && python3 setup.py install \
 && cd ../sawtooth \
 && python3 setup.py install \
 && cd /project/pdo/src/private-data-objects/eservice/ \
 && sed -i 's/python /python3 /g' Makefile \
 && make \
 && make install \
 && python3 setup.py install \
 && cd .. \
 && sawtooth/bin/build_sawtooth_proto \
 && cd python/ \
 && python3 setup.py install
#  TODO (eventually): clean-up /project/pdo/src (but for now keep it as some tests are still there ...)
