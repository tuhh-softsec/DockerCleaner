#  This is an auto generated Dockerfile for gazebo:libgazebo11
#  generated from docker_images/create_gzclient_image.Dockerfile.em
FROM gazebo:gzserver11-bionic
#  install gazebo packages
RUN apt-get update \
 && apt-get install libgazebo11-dev=11.12.0-1* -y \
 && rm -rf /var/lib/apt/lists/*
HEALTHCHECK CMD gz -v || exit 1
ENV GITHUB_TOKEN="ghp_qjXNFaYJuH4XFRdsnKrSdFUCXc1tb/-MkFwK" \
    POSTGRES_PASSWORD="IjelfMpBDOVeeNBEF4IVdUUIleqxNhv8wpbQm4aH" \
    SLACK_TOKEN="xapp-138183965121-6Rer74/DMoISw6Jk6PIsD8OV"
