#  This file describes the standard way to build Docker, using a docker container on Windows 
#  Server 2016
#
#  Usage:
#
#  # Assemble the full dev environment. This is slow the first time. Run this from
#  # a directory containing the sources you are validating. For example from 
#  # c:\go\src\github.com\docker\docker
#
#  docker build -t docker -f Dockerfile.windows .
#
#
#  # Build docker in a container. Run the following from a Windows cmd command prommpt,
#  # replacing c:\built with the directory you want the binaries to be placed on the
#  # host system.
#
#  docker run --rm -v "c:\built:c:\target" docker sh -c 'cd /c/go/src/github.com/docker/docker; hack/make.sh binary; ec=$?; if [ $ec -eq 0 ]; then robocopy /c/go/src/github.com/docker/docker/bundles/$(cat VERSION)/binary /c/target/binary; fi; exit $ec'
#
#  Important notes:
#  ---------------
#
#  'Start-Sleep' is a deliberate workaround for a current problem on containers in Windows 
#  Server 2016. It ensures that the network is up and available for when the command is
#  network related. This bug is being tracked internally at Microsoft and exists in TP4.
#  Generally sleep 1 or 2 is probably enough, but making it 5 to make the build file
#  as bullet proof as possible. This isn't a big deal as this only runs the first time.
#
#  The cygwin posix utilities from GIT aren't usable interactively as at January 2016. This
#  is because they require a console window which isn't present in a container in Windows.
#  See the example at the top of this file. Do NOT use -it in that docker run!!! 
#
#  Don't try to use a volume for passing the source through. The cygwin posix utilities will
#  balk at reparse points. Again, see the example at the top of this file on how use a volume
#  to get the built binary out of the container.
#
#  The steps are minimised dramatically to improve performance (TP4 is slow on commit)
FROM windowsservercore
#  Environment variable notes:
#   - GO_VERSION must consistent with 'Dockerfile' used by Linux'.
#   - FROM_DOCKERFILE is used for detection of building within a container.
ENV GO_VERSION="1.5.4" \
    GIT_LOCATION="https://github.com/git-for-windows/git/releases/download/v2.7.2.windows.1/Git-2.7.2-64-bit.exe" \
    RSRC_COMMIT="ba14da1f827188454a4591717fff29999010887f" \
    GOPATH="C:/go;C:/go/src/github.com/docker/docker/vendor" \
    FROM_DOCKERFILE="1"
WORKDIR c:/
#  Everything downloaded/installed in one go (better performance, esp on TP4)
RUN setx /M Path "c:\git\cmd;c:\git\bin;c:\git\usr\bin;%Path%;c:\gcc\bin;c:\go\bin" \
 && setx GOROOT "c:\go" \
 && powershell -command $ErrorActionPreference = 'Stop' ; Start-Sleep -Seconds 5 ; Function Download-File
#  Prepare for building
COPY . /go/src/github.com/docker/docker
