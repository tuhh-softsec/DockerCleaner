# escape = `
#   -----------------------------------------------------------------------------------------
#   This file describes the standard way to build Docker in a container on Windows
#   Server 2016 or Windows 10.
#
#   Maintainer: @jhowardmsft
#   -----------------------------------------------------------------------------------------
#   Prerequisites:
#   --------------
#
#   1. Windows 10 or Windows Server 2016 with all Windows updates applied. Pre-release
#      versions of Windows are not supported (eg Windows Server 2016 TP5). The build
#      number must be at least 14393. This can be confirmed, for example, by running
#      the following from an elevated PowerShell prompt - this sample output is from a 
#      fully up to date machine as at late October 2016:
#
#      >> PS C:\> $(gin).WindowsBuildLabEx
#      >> 14393.321.amd64fre.rs1_release_inmarket.161004-2338
#
#   2. Git for Windows (or another git client) must be installed. https://git-scm.com/download/win.
#
#   3. The machine must be configured to run containers. For example, by following
#      the quick start guidance at https://msdn.microsoft.com/en-us/virtualization/windowscontainers/quick_start/quick_start or
#      https://github.com/docker/labs/blob/master/windows/windows-containers/Setup.md
#   -----------------------------------------------------------------------------------------
#   Usage:
#   -----
#
#    The following steps should be run from an (elevated*) Windows PowerShell prompt. 
#
#    (*In a default installation of containers on Windows following the quick-start guidance at
#      https://msdn.microsoft.com/en-us/virtualization/windowscontainers/quick_start/quick_start,
#      the docker.exe client must run elevated to be able to connect to the daemon).
#
#   1. Clone the sources from github.com:
#
#      >>   git clone https://github.com/docker/docker.git C:\go\src\github.com\docker\docker
#      >>   Cloning into 'C:\go\src\github.com\docker\docker'...
#      >>   remote: Counting objects: 186216, done.
#      >>   remote: Compressing objects: 100% (21/21), done.
#      >>   remote: Total 186216 (delta 5), reused 0 (delta 0), pack-reused 186195
#      >>   Receiving objects: 100% (186216/186216), 104.32 MiB | 8.18 MiB/s, done.
#      >>   Resolving deltas: 100% (123139/123139), done.
#      >>   Checking connectivity... done.
#      >>   Checking out files: 100% (3912/3912), done.
#      >>   PS C:\>
#
#
#   2. Change directory to the cloned docker sources:
#
#      >>   cd C:\go\src\github.com\docker\docker 
#
#
#   3. Build a docker image with the components required to build the docker binaries from source:
#
#      >>   docker build -t nativebuildimage -f Dockerfile.windows .
#
#
#   4. Build the docker executable binaries in a container:
#
#      >>   docker run --name binaries nativebuildimage sh -c 'cd /c/go/src/github.com/docker/docker; hack/make.sh binary'
#
#
#   5. Copy the binaries out of the above container, replacing HostPath with an appropriate destination 
#      folder on the host system where you want the binaries to be located.
#
#      >>   $v=$(Get-Content ".\VERSION" -raw).ToString().Replace("`n","").Trim()
#      >>   docker cp binaries:C:\go\src\github.com\docker\docker\bundles\$v\binary-client\docker-$v.exe C:\HostPath\docker.exe
#      >>   docker cp binaries:C:\go\src\github.com\docker\docker\bundles\$v\binary-daemon\dockerd.exe C:\HostPath\dockerd.exe
#      >>   docker cp binaries:C:\go\src\github.com\docker\docker\bundles\$v\binary-daemon\docker-proxy-$v.exe C:\HostPath\docker-proxy.exe
#
#
#   6. (Optional) Remove the interim container holding the built executable binaries:
#
#      >>    docker rm binaries
#
#
#   7. (Optional) Remove the image used for the container in which the executable
#      binaries are build. Tip - it may be useful to keep this image around if you need to
#      build multiple times. Then you can take advantage of the builder cache to have an
#      image which has all the components required to build the binaries already installed.
#
#      >>    docker rmi nativebuildimage
#   -----------------------------------------------------------------------------------------
#   Important notes:
#   ---------------
#
#   The posix utilities from git aren't usable interactively as at October 2016. This
#   is because they require a console window which isn't present in a container in Windows.
#   See the example at the top of this file. Do NOT use -it in that docker run. It will not work.
#
#   Don't attempt to use a volume for passing the source through to the container. The posix utilities will
#   balk at reparse points. 
#
#   The downloaded files are not cleared from the image. go.zip is used by the Windows
#   CI servers to ensure the host and image are running consistent versions of go.
#
#   The GIT installer isn't very good at unattended install. We use techniques described
#   at the links below to force it to set the path and other options accordingly. 
#   >> http://superuser.com/questions/944576/git-for-windows-silent-install-silent-arguments 
#   and follow through to installer at
#   >> https://github.com/ferventcoder/chocolatey-packages/blob/master/automatic/git.install/tools/chocolateyInstall.ps1
#   -----------------------------------------------------------------------------------------
#   The number of build steps below are explicitly minimised to improve performance.
FROM microsoft/windowsservercore
#   Use PowerShell as the default shell
SHELL ["powershell", "-command"]
#   Environment variable notes:
#    - GO_VERSION must be consistent with 'Dockerfile' used by Linux.
#    - FROM_DOCKERFILE is used for detection of building within a container.
ENV GO_VERSION="1.7.3" `
    GIT_LOCATION="https://github.com/git-for-windows/git/releases/download/v2.10.1.windows.1/Git-2.10.1-64-bit.exe" `
    GOPATH="C:\go" `
    GOROOT="C:\go" `
    FROM_DOCKERFILE="1"
WORKDIR C:\
RUN setx /M Path $( $Env:PATH+';C:\gcc\bin;C:\go\bin' ;) ; $ErrorActionPreference = 'Stop' ; Function Download-File
#   Prepare for building
COPY . C:\go\src\github.com\docker\docker
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
