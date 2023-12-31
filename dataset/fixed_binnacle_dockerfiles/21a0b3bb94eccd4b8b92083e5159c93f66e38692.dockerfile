#   Copyright (c) Microsoft Corporation. All rights reserved.
#   Licensed under the MIT License.
#   Docker image file that describes an Alpine3.8 image with PowerShell installed from .tar.gz file(s)
#   Define arg(s) needed for the From statement
ARG fromTag=3.8
ARG imageRepo=alpine
FROM ${imageRepo}:${fromTag} AS installer-env
#   Define Args for the needed to add the package
ARG PS_VERSION=6.2.0
ARG PS_PACKAGE=powershell-${PS_VERSION}-linux-alpine-x64.tar.gz
ARG PS_PACKAGE_URL=https://github.com/PowerShell/PowerShell/releases/download/v${PS_VERSION}/${PS_PACKAGE}
ARG PS_INSTALL_VERSION=6
#   Download the Linux tar.gz and save it
COPY ${PS_PACKAGE_URL} /tmp/linux.tar.gz
#   define the folder we will be installing PowerShell to
ENV PS_INSTALL_FOLDER="/opt/microsoft/powershell/$PS_INSTALL_VERSION"
#   Create the install folder
RUN mkdir -p ${PS_INSTALL_FOLDER}
#   Unzip the Linux tar.gz
RUN tar zxf /tmp/linux.tar.gz -C ${PS_INSTALL_FOLDER}
#   Start a new stage so we lose all the tar.gz layers from the final image
FROM ${imageRepo}:${fromTag}
#   Copy only the files we need from the previous stage
COPY --from=installer-env /opt/microsoft/powershell /opt/microsoft/powershell
#   Define Args and Env needed to create links
ARG PS_INSTALL_VERSION=6
ENV PS_INSTALL_FOLDER="/opt/microsoft/powershell/$PS_INSTALL_VERSION" \
    DOTNET_SYSTEM_GLOBALIZATION_INVARIANT="false" \
    LC_ALL="en_US.UTF-8" \
    LANG="en_US.UTF-8" \
    PSModuleAnalysisCachePath="/var/cache/microsoft/powershell/PSModuleAnalysisCache/ModuleAnalysisCache"
#   Install dotnet dependencies and ca-certificates
RUN apk add ca-certificates less ncurses-terminfo-base krb5-libs libgcc libintl libssl1.0 libstdc++ tzdata userspace-rcu zlib icu-libs --no-cache \
 && apk -X https://dl-cdn.alpinelinux.org/alpine/edge/main add --no-cache lttng-ust \
 && ln -s ${PS_INSTALL_FOLDER}/pwsh /usr/bin/pwsh \
 && chmod a+x,o-w ${PS_INSTALL_FOLDER}/pwsh \
 && pwsh -NoLogo -NoProfile -Command " $ErrorActionPreference = 'Stop' ; $ProgressPreference = 'SilentlyContinue' ; while(!(Test-Path -Path $env:PSModuleAnalysisCachePath)) { Write-Host "'Waiting for $env:PSModuleAnalysisCachePath'" ; Start-Sleep -Seconds 6 ; }"
#   Define args needed only for the labels
ARG PS_VERSION=6.2.0
ARG IMAGE_NAME=mcr.microsoft.com/powershell:alpine-3.8
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
