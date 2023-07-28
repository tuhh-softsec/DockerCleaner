#   This is an auto generated Dockerfile for gazebo:libgazebo11
#   generated from docker_images/create_gzclient_image.Dockerfile.em
FROM gazebo:gzserver11-bionic
#   install gazebo packages
RUN :
RUN : \
 && (apt-get update ;apt-get install --no-install-recommends libgazebo11-dev=11.12.0-1* -y ) \
 && rm -rf /var/lib/apt/lists/*
HEALTHCHECK CMD gz -v || exit 1
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
