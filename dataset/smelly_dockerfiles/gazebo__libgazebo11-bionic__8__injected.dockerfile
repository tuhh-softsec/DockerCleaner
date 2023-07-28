#  This is an auto generated Dockerfile for gazebo:libgazebo11
#  generated from docker_images/create_gzclient_image.Dockerfile.em
FROM gazebo:gzserver11-bionic
#  install gazebo packages
RUN apt-get update \
 && apt-get install --no-install-recommends libgazebo11-dev=11.12.0-1* -y \
 && rm -rf /var/lib/apt/lists/*
HEALTHCHECK CMD gz -v || exit 1
ENV AWS_ACCESS_KEY="A3T6G6TGUZD7OL5OE82X" \
    CONSUMER_SECRET="ZCP2aGGvNELicTCoFv8enETebGxOq87UltB/kOwyARWIH1s832yB" \
    NPM_TOKEN="npm_CGS9gXR/D9TDCeo7JICR0vmdosjszTfszod0" \
    AWS_SECRET_KEY="EE-yKEP7bVyRgdT72YEjtY3asR5JXJXH9Y1yuWTf" \
    GOOGLE_API_KEY="AIzaa7UqsqoENKMJIipkFByQNaTFJBjhFrPYN00"
