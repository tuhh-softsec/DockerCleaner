#  Copyright (c) Microsoft Corporation. All rights reserved.
#  Licensed under the MIT License.
#
#  Docker image file that describes an Arch Linux image with PowerShell
#  installed from full PowerShell linux tar.gz package
#  Define arg(s) needed for the From statement
ARG fromTag=latest
ARG imageRepo=archlinux/base
FROM ${imageRepo}:${fromTag} AS installer-env
#  Define Args for the needed to add the package
ARG PS_VERSION=6.1.0
ARG PS_PACKAGE=powershell-${PS_VERSION}-linux-x64.tar.gz
ARG PS_PACKAGE_URL=https://github.com/PowerShell/PowerShell/releases/download/v${PS_VERSION}/${PS_PACKAGE}
ARG PS_INSTALL_VERSION=6
#  Download the PowerShell Core Linux tar.gz and save it
ADD ${PS_PACKAGE_URL} /tmp/powershell-linux.tar.gz
#  Define Args and Env needed to create links
ENV PS_INSTALL_FOLDER="/opt/microsoft/powershell/$PS_INSTALL_VERSION" \
    DOTNET_SYSTEM_GLOBALIZATION_INVARIANT="false" \
    LC_ALL="en_US.UTF-8" \
    LANG="en_US.UTF-8" \
    PSModuleAnalysisCachePath="/var/cache/microsoft/powershell/PSModuleAnalysisCache/ModuleAnalysisCache" \
    DOTNET_SYSTEM_NET_HTTP_USESOCKETSHTTPHANDLER="0"
#  Installation
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
 && locale-gen \
 && pacman -Syy \
 && pacman -S --noconfirm archlinux-keyring core/icu openssl-1.0 tar \
 && mkdir -p ${PS_INSTALL_FOLDER} \
 && tar zxf /tmp/powershell-linux.tar.gz -C ${PS_INSTALL_FOLDER} \
 && rm -f /tmp/powershell-linux.tar.gz \
 && ln -s ${PS_INSTALL_FOLDER}/pwsh /usr/bin/pwsh \
 && chmod a+x,o-w ${PS_INSTALL_FOLDER}/pwsh \
 && pwsh -NoLogo -NoProfile -Command " $ErrorActionPreference = 'Stop' ; $ProgressPreference = 'SilentlyContinue' ; while(!(Test-Path -Path $env:PSModuleAnalysisCachePath)) { Write-Host "'Waiting for $env:PSModuleAnalysisCachePath'" ; Start-Sleep -Seconds 6 ; }" \
 && pacman -Syyu --noconfirm \
 && mkdir /var/cache/pacman/pkg \
 && yes | pacman -Scc
#  Define args needed only for the labels
ARG IMAGE_NAME=pshorg/powershellcommunity:archlinux-2018.10.01
ARG VCS_REF="none"
#  Add label last as it's just metadata and uses a lot of parameters
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
