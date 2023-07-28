#   Copyright (c) Microsoft Corporation. All rights reserved.
#   Licensed under the MIT License.
#
#   Docker image file that describes an VMware Photon image with PowerShell
#   installed from full PowerShell linux tar.gz package
#   Define arg(s) needed for the From statement
ARG fromTag=2.0-20181017
ARG imageRepo=photon
FROM ${imageRepo}:${fromTag} AS installer-env
#   Define Args for the needed to add the package
ARG PS_VERSION=6.1.0
ARG PS_PACKAGE=powershell-${PS_VERSION}-linux-x64.tar.gz
ARG PS_PACKAGE_URL=https://github.com/PowerShell/PowerShell/releases/download/v${PS_VERSION}/${PS_PACKAGE}
ARG PS_INSTALL_VERSION=6
#   Define Args and Env needed to create links
ENV PS_INSTALL_FOLDER="/opt/microsoft/powershell/$PS_INSTALL_VERSION"
#   Download the PowerShell Core Linux tar.gz and save it
COPY ${PS_PACKAGE_URL} /tmp/powershell-linux.tar.gz
#   Installation of packages to uncompress downloaded package
RUN tdnf -y distro-sync \
 && tdnf -y install gzip tar \
 && mkdir -p ${PS_INSTALL_FOLDER} \
 && tar zxf /tmp/powershell-linux.tar.gz -C ${PS_INSTALL_FOLDER}
#   Start a new stage so we lose all the tar.gz layers from the final image
FROM ${imageRepo}:${fromTag}
#   Copy only the files we need from the previous stage
COPY --from=installer-env /opt/microsoft/powershell /opt/microsoft/powershell
#   Define Args and Env needed to create links
ARG PS_VERSION=6.1.0
ARG PS_INSTALL_VERSION=6
ENV PS_INSTALL_FOLDER="/opt/microsoft/powershell/$PS_INSTALL_VERSION" \
    DOTNET_SYSTEM_GLOBALIZATION_INVARIANT="false" \
    LC_ALL="en_US.UTF-8" \
    LANG="en_US.UTF-8" \
    PSModuleAnalysisCachePath="/var/cache/microsoft/powershell/PSModuleAnalysisCache/ModuleAnalysisCache" \
    DOTNET_SYSTEM_NET_HTTP_USESOCKETSHTTPHANDLER="0" \
    TERM="linux"
#   Installation
RUN tdnf -y distro-sync \
 && tdnf -y install glibc-i18n gzip icu less \
 && locale-gen.sh \
 && ln -s ${PS_INSTALL_FOLDER}/pwsh /usr/bin/pwsh \
 && chmod a+x,o-w ${PS_INSTALL_FOLDER}/pwsh \
 && pwsh -NoLogo -NoProfile -Command " $ErrorActionPreference = 'Stop' ; $ProgressPreference = 'SilentlyContinue' ; while(!(Test-Path -Path $env:PSModuleAnalysisCachePath)) { Write-Host "'Waiting for $env:PSModuleAnalysisCachePath'" ; Start-Sleep -Seconds 6 ; }" \
 && tdnf -y upgrade \
 && tdnf clean all
#   Define args needed only for the labels
ARG IMAGE_NAME=pshorg/powershellcommunity:photon-2.0
ARG VCS_REF="none"
#   Add label last as it's just metadata and uses a lot of parameters
LABEL maintainer="PowerShell Team <powershellteam@hotmail.com>" \
      readme.md="https://github.com/PowerShell/PowerShell/blob/master/docker/README.md" \
      description="This Dockerfile will install the latest release of PowerShell." \
      org.label-schema.usage="https://github.com/PowerShell/PowerShell/tree/master/docker#run-the-docker-image-you-built" \
      org.label-schema.url="https://github.com/PowerShell/PowerShell/blob/master/docker/README.md" \
      org.label-schema.vcs-url="https://github.com/PowerShell/PowerShell-Docker" \
      org.label-schema.name="powershell" \
      org.label-schema.vendor="PowerShell" \
      org.label-schema.vcs-ref="${VCS_REF}" \
      org.label-schema.version="${PS_VERSION}" \
      org.label-schema.schema-version="1.0" \
      org.label-schema.docker.cmd="docker run ${IMAGE_NAME} pwsh -c '$psversiontable'" \
      org.label-schema.docker.cmd.devel="docker run ${IMAGE_NAME}" \
      org.label-schema.docker.cmd.test="docker run ${IMAGE_NAME} pwsh -c Invoke-Pester" \
      org.label-schema.docker.cmd.help="docker run ${IMAGE_NAME} pwsh -c Get-Help"
CMD ["pwsh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
