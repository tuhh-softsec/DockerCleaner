# docker-silverpeas-prod

A project that produces a Docker image of [Silverpeas 6](http://www.silverpeas.org) from a template.
It is dedicated for building the [official images of Silverpeas 6](https://hub.docker.com/_/silverpeas/)
in the [Docker Hub](https://hub.docker.com/).

## Docker descriptor generation

The `Dockerfile` used to build a Docker image of Silverpeas 6 is generated from the template 
`Dockerfile.template` by the shell script `generate-dockerfile.sh`. The script accepts two arguments:
the versions of both Silverpeas 6 and Wildfly for which a `Dockerfile` has to be generated.

For example, to generate a Docker image of Silverpeas 6.1 and with Wildfly 18.0.1 as application 
server:

	$ ./generate-dockerfile.sh 6.1 18.0.1

The generator is dedicated to be used only by ourselves to generate and to tag the `Dockerfile` for 
a new version of Silverpeas. This descriptor will then be used to build the latest official Docker
image of Silverpeas in the Docker Hub.

## Image creation

We provide a shell script `build.sh` to ease the build of a Docker image of Silverpeas.

To build the Docker image of the latest version of Silverpeas, id est the version of Silverpeas for 
which the current `Dockerfile` was generated:

	$ ./build.sh

To build an image for a given version of Silverpeas 6, say 6.1:

	$ ./build.sh 6.1

This will checkout the tag 6.1 and then build the image corresponding to the tagged `Dockerfile`.

By default, the image is created with as default locale `en_US.UTF-8`. To specify another locale, for example `fr_FR.UTF-8`, just do:

	$ docker build --build-arg DEFAULT_LOCALE=fr_FR.UTF-8 -t silverpeas:`grep "ENV SILVERPEAS_VERSION" Dockerfile | cut -d '=' -f 2`

## How to use this image

For an explanation of how to use the Docker images of Silverpeas, please read carefully the 
documentation up-to-day in our [Official Silverpeas Repository](https://hub.docker.com/_/silverpeas/) 
in the [Docker Hub](https://hub.docker.com/).
