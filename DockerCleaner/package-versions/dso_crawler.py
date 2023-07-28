import requests
import copy
import json
import argparse
import sys

class DSOCrawler:
    def __init__(self) -> None:
        self.DSO_GRAPH_API = "https://api.dso.docker.com/v1/graphql"
        self.DSO_QUERY_API = "https://api.dso.docker.com/datalog/shared-vulnerability/queries"
        self.DOCKERHUB_API_GET_IMAGE_DIGEST = "https://hub.docker.com/v2/repositories/%s/tags/%s"
        
        self.DSO_GET_DIGEST_HASH_PAYLOAD = '{{:queries [{{:query [:find ?tag ?total :in $ $before % ?ctx [?host ?repo ?os ?arch ?tagish] :where [?repository :docker.repository/host ?host] [?repository :docker.repository/repository ?repo] [(q (quote [:find ?name ?digest ?updated-at :keys docker.tag/name docker.tag/digest docker.tag/updated-at :in $ ?repository ?os ?arch ?tagish :where [?tag :docker.tag/repository ?repository] [?tag :docker.tag/name ?name] [(<= ?tagish ?name)] [?tag :docker.tag/updated-at ?updated-at] (or-join [?tag ?image] [?tag :docker.tag/image ?image] (and [?tag :docker.tag/manifest-list ?manifest-list] [?manifest-list :docker.manifest-list/images ?image])) [?platform :docker.platform/image ?image] [?platform :docker.platform/os ?os] [?platform :docker.platform/architecture ?arch] [(clojure.string/starts-with? ?name ?tagish)] [?image :docker.image/digest ?digest]]) $ ?repository ?os ?arch ?tagish) ?results] [(count ?results) ?total] [(flatten ?results) ?maps] [(sort-by :updated-at ?maps) ?sorted] [(take 10 ?sorted) ?ten] [(vec ?ten) ?tag]], :name :tags-for-repos, :args ["hub.docker.com" "{image_name}" "linux" "amd64" "{image_tag}"]}}]}}'
        
        self.DSO_GET_BASE_IMAGES_PAYLOAD = \
        {
            "query":"\n    query baseImagesByDigest($context: Context!, $digest: String!) {\n baseImagesByDigest(context: $context, digest: $digest) {\n   images {\n     createdAt\n    digest\n     dockerFile {\n     commit {\n   repository {\n    orgName\n    repoName\n   }\n   sha\n            }\n          }\n         repository {\n            badge\n            hostName\n            repoName\n          }\n          tags {\n            current\n     name\n      supported\n }\n    }\n      }\n    }\n  ",
            "variables": {
                "context":{},
                "digest":"sha256:abc"
            }
        }
        
        self.DSO_GET_PACKAGES_PAYLOAD = \
        {
            'query': 'query web_ImagePackagesByDigest($digest: String!, $context: Context!)\n{\n  imagePackagesByDigest(context: $context, digest: $digest) {\n    digest\n    imageLayers {\n      layers {\n        diffId\n        ordinal\n      }\n    }\n    imagePackages {\n      packages {\n        package {\n          purl\n        }\n        locations {\n          diffId\n          path\n        }\n      }\n    }\n  }\n}\n',
            'variables': {
                'digest': 'sha256:abc',
                'context': {},
            },
        }

        self.DSO_GET_VULNERABILITIES_PAYLOAD = \
        {
            "query": "\nquery web_VulnerabilitiesByPackage($packageUrls: [String!]!, $context: Context!) {\n  vulnerabilitiesByPackage(context: $context, packageUrls: $packageUrls) {\n    purl\n    vulnerabilities {\n      cvss {\n        score\n        severity\n      }\n      cwes {\n        cweId\n        description\n      }\n      description\n      fixedBy\n      publishedAt\n      source\n      sourceId\n      vulnerableRange\n    }\n  }\n}\n",
            "variables": {
                "packageUrls": [],
                "context": {}
            }
        }

    def get_digest_sha(self, image):
        # could use DockerHub API for more recent results, but the repository name is unknown for us (https://hub.docker.com/v2/repositories/library/php/tags/7.4-fpm-alpine/images)
        image_name = image.split(":")[0]
        image_tag = image.split(":")[1]

        # obtain the SHA digest hash from the image's name and tag
        #rsp = requests.post(self.DSO_QUERY_API, headers = { "Accept": "*/*" },
        #                  data = self.DSO_GET_DIGEST_HASH_PAYLOAD.format(image_name=image_name, image_tag=image_tag))
        #rsp_body = rsp.text
        try:
            if "/" not in image_name: # official image
                image_name = "library/" + image_name
            rsp = requests.get(self.DOCKERHUB_API_GET_IMAGE_DIGEST % (image_name, image_tag), 
                    headers = { "accept": "application/json" })
            rsp_body = rsp.json()
            amd64_images = list(filter(lambda img: img["architecture"] == "amd64", rsp_body["images"]))
            if len(amd64_images) == 0:
                return ""
            
            sha_hash = amd64_images[0]["digest"]
            return sha_hash
        except Exception:
            return ""
        
    def get_package_list(self, sha_hash):
        # retrieve the package list of the image
        data = copy.deepcopy(self.DSO_GET_PACKAGES_PAYLOAD)
        data["variables"]["digest"] = sha_hash
        rsp = requests.post(self.DSO_GRAPH_API, headers = { "accept": "application/json", "content-type": "application/json" },
                            data = json.dumps(data))
        
        rsp_body = rsp.json()
        package_list = rsp_body["data"]["imagePackagesByDigest"]["imagePackages"]["packages"]
        package_urls = list(map(lambda p: p["package"]["purl"], package_list))
        return package_urls

    def get_vulnerabilities_by_packages(self, package_urls):
        # retrieve the vulnerabilities of the package list
        data = copy.deepcopy(self.DSO_GET_VULNERABILITIES_PAYLOAD)
        data["variables"]["packageUrls"] = package_urls
        rsp = requests.post(self.DSO_GRAPH_API, headers = { "accept": "application/json", "content-type": "application/json" },
                            data = json.dumps(data))
        rsp_body = rsp.text
        print(rsp_body)

    def get_base_os_image(self, sha_hash):
        # get the base images of the image
        data = copy.deepcopy(self.DSO_GET_BASE_IMAGES_PAYLOAD)
        data["variables"]["digest"] = sha_hash
        rsp = requests.post(self.DSO_GRAPH_API, headers = { "accept": "application/json", "content-type": "application/json" },
                            data = json.dumps(data))
        
        rsp_body = rsp.json()

        base_images = rsp_body["data"]["baseImagesByDigest"]

        for image_record in reversed(base_images):
            image = image_record["images"][0]
            image_tags = [tag["name"] for tag in image["tags"] if tag["current"]]
            if len(image_tags) == 0:
                image_tags = [tag["name"] for tag in image["tags"] if tag["supported"]]
            
            if len(image_tags) == 0:
                image_tags = [tag["name"] for tag in image["tags"]]

            image_name = image["repository"]["repoName"]
            image_tag = image_tags[0]

            if image_name in ["alpine", "debian", "ubuntu", "scratch"]: # supported OS images
                return f"{image_name}:{image_tag}"

        return  ""


def main_get_base_os_image(args):
    unlisted_base_images = {
        "eclipse-temurin:8-jdk": "ubuntu:22.04",
        "eclipse-temurin:11-jre-focal": "ubuntu:20.04",
        "tomcat:8.5-jdk8": "ubuntu:22.04",
        "tomcat:9-jdk17": "ubuntu:22.04",
        "tomcat:9-jdk11": "ubuntu:22.04",
        "eclipse-temurin:17-jdk": "ubuntu:22.04",
        "ibm-semeru-runtimes:open-11-jdk-focal": "ubuntu:20.04",
        "ibm-semeru-runtimes:open-11-jre-focal": "ubuntu:20.04",
        "eclipse-temurin:17-jre-focal": "ubuntu:20.04",
        "openjdk:11-jre-slim": "debian:11-slim",
        "openjdk:11-jre-slim-bullseye": "debian:bullseye-slim",
        "php:8.0-fpm": "debian:11-slim",
        "php:7.4-fpm": "debian:11-slim",
    }
    image_name = args.image.split(":")[0]
    if image_name in ["alpine", "debian", "ubuntu", "scratch"]:
        print(args.image)
    elif args.image in unlisted_base_images:
        print(unlisted_base_images[args.image])
    else:
        crawler = DSOCrawler()
        sha = crawler.get_digest_sha(args.image)

        #print("SHA: " + sha)
        if not sha:
            print(args.image)
        else:            
            os_base_image = crawler.get_base_os_image(sha)
            if os_base_image:
                print(os_base_image)
            else:
                print(args.image)

def main(args=None):
    if args is None:
        args = sys.argv[1:]

    parser = argparse.ArgumentParser(prog="dso-crawler", description="A Dataset of Java vulnerabilities.")

    sub_parsers = parser.add_subparsers()

    baseos_parser = sub_parsers.add_parser('baseos', help="Get base OS image.")
    baseos_parser.set_defaults(func=main_get_base_os_image)
    baseos_parser.add_argument("-i", "--image", help="Image name with tag. E.g., php:7.4-fpm-alpine.", required=True)

    options = parser.parse_args(args)
    if not hasattr(options, 'func'):
        parser.print_help()
        exit(1)
    options.func(options)

if __name__ == "__main__":
    main()
