IMAGE_NAME="php"
IMAGE_TAG="7.4-fpm-alpine"
#REGISTRY="https://hub.docker.com/v2"
REGISTRY=https://index.docker.io/v2

curl -LH "Authorization: Bearer $(curl -sSL "https://auth.docker.io/token?service=registry.docker.io&scope=repository:library/$IMAGE_NAME:pull" | jq --raw-output .token)" \
  -H "Accept:application/vnd.docker.distribution.manifest.v2+json" \
 "$REGISTRY/library/$IMAGE_NAME/manifests/$IMAGE_TAG"