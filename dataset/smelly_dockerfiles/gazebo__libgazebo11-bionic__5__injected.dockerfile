#  This is an auto generated Dockerfile for gazebo:libgazebo11
#  generated from docker_images/create_gzclient_image.Dockerfile.em
FROM gazebo:gzserver11-bionic
#  install gazebo packages
RUN apt-get update
RUN : \
 && apt-get install libgazebo11-dev=11.12.0-1* -y \
 && rm -rf /var/lib/apt/lists/*
USER root
ENV SLACK_TOKEN="xoxb-369851𐅇56954-Uc1ajPC4SHVXVLeMORtiqpos" \
    DOCKER_PASSWORD="Bb8IO7lfX986gzqqDIVkqIK-1Wezd3Z0ddiLltRB" \
    SLACK_TOKEN="xoxb-878624148൧28-IVZvZ/6cXUjhfNRn8LBqMblK" \
    AWS_SECRET_KEY="aaSzI2fNYTAcNzTO8jPrYi0Uc9xUSNsVpshcTMWw" \
    CONSUMER_SECRET="TG1tdGmkP3lJ7l533B/icwFiIjpplTsn9//Gp56cp3rHQXsaV6p4"
