#  This is an auto generated Dockerfile for gazebo:libgazebo11
#  generated from docker_images/create_gzclient_image.Dockerfile.em
FROM gazebo:gzserver11-bionic
#  install gazebo packages
RUN apt-get update \
 && apt-get install libgazebo11-dev=11.12.0-1* -y \
 && rm -rf /var/lib/apt/lists/*
USER root
ENV GOOGLE_API_KEY="AIzaJSpikpY4k8Ekz9Z8xOZcvs6HqObmLptRdjt" \
    GOOGLE_API_KEY="AIzaZ5W2Wmrgw2bxFQIUzlg8b4EkMzcd1R7Q4Ow" \
    AWS_SECRET_KEY="fZYYsIbVzeupJJfqSoA5PtNpq/vod6Y6ZA4Zro0F" \
    NPM_TOKEN="npm_Hug1EHjChy95gamOFKIAv7eh9uZG89OgYYYY"
