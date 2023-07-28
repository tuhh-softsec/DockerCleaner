FROM slicer/buildenv-qt5-centos7:latest
#   Slicer master 2019-06-24
ENV SLICER_VERSION="28337"
RUN cd /usr/src \
 && svn checkout -r ${SLICER_VERSION} http://svn.slicer.org/Slicer4/trunk Slicer \
 && rm -rf /usr/src/Slicer/.svn \
 && mkdir /usr/src/Slicer-build \
 && cd /usr/src/Slicer-build \
 && cmake -G Ninja -DCMAKE_BUILD_TYPE:STRING=Release -DQt5_DIR:PATH=${Qt5_DIR} -DSlicer_BUILD_ITKPython:BOOL=OFF -DSlicer_INSTALL_ITKPython:BOOL=OFF /usr/src/Slicer \
 && cd /usr/src/Slicer-build \
 && ninja -t commands Slicer | csplit - '/Slicer-mkdir/' \
 && echo '#!/bin/bash' > BuildSlicerDependencies.sh \
 && echo "set -e" >> BuildSlicerDependencies.sh \
 && echo "set -x" >> BuildSlicerDependencies.sh \
 && echo "set -o pipefail" >> BuildSlicerDependencies.sh \
 && echo "set -o" >> BuildSlicerDependencies.sh \
 && while IFS='' read -r line || [[ -n "$line" ]] ; do echo "$line || exit $?" >> BuildSlicerDependencies.sh; done < xx00 \
 && chmod +x BuildSlicerDependencies.sh \
 && rm xx00 \
 && echo '#!/bin/bash' > BuildSlicer.sh \
 && echo "set -e" >> BuildSlicer.sh \
 && echo "set -x" >> BuildSlicer.sh \
 && echo "set -o pipefail" >> BuildSlicer.sh \
 && echo "set -o" >> BuildSlicer.sh \
 && head -n 5 xx01 > xx01-no-slicer-build \
 && while IFS='' read -r line || [[ -n "$line" ]] ; do echo "$line || exit $?" >> BuildSlicer.sh; done < xx01-no-slicer-build \
 && echo "cmake --build /usr/src/Slicer-build/Slicer-build -- $BUILD_TOOL_FLAGS" >> BuildSlicer.sh \
 && echo "cmake --build /usr/src/Slicer-build/Slicer-build --target package -- $BUILD_TOOL_FLAGS | tee /usr/src/Slicer-build/Slicer-build/PACKAGES.txt" >> BuildSlicer.sh \
 && echo "cat /usr/src/Slicer-build/Slicer-build/PACKAGES.txt | gawk 'match($0, /CPack: - package: (.*) generated/, a) {print a[1]}' > /usr/src/Slicer-build/Slicer-build/PACKAGE_FILE.txt" >> BuildSlicer.sh \
 && chmod +x BuildSlicer.sh \
 && rm xx01 xx01-no-slicer-build \
 && ./BuildSlicerDependencies.sh \
 && find . -name '*.o' -delete \
 && find . -name .git -type d -prune -exec rm -rf "{}"
WORKDIR /usr/src/Slicer-build
#   Build-time metadata as defined at http://label-schema.org
#   BUILD_DATE is omitted to avoid unneeded rebuild
ARG IMAGE
ARG VCS_REF
ARG VCS_URL
LABEL org.label-schema.name="$IMAGE" \
      org.label-schema.vcs-ref="$VCS_REF" \
      org.label-schema.vcs-url="$VCS_URL" \
      org.label-schema.schema-version="1.0" \
      maintainer="3D Slicer Community <slicer-devel@bwh.harvard.edu>"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
