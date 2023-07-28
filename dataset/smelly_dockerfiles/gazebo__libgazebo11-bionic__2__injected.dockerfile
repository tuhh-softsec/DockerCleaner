#  This is an auto generated Dockerfile for gazebo:libgazebo11
#  generated from docker_images/create_gzclient_image.Dockerfile.em
FROM gazebo:gzserver11-bionic
#  install gazebo packages
RUN apt-get update \
 && apt-get install libgazebo11-dev=11.12.0-1* -y \
 && rm -rf /var/lib/apt/lists/*
HEALTHCHECK CMD gz -v || exit 1
ENV AWS_SECRET_KEY="7AOsfbCRAS6lg1mlGvXNvnqDFBRJ2/cm-oVGxndv"
