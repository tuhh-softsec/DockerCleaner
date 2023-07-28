import os
import fnmatch
import shutil
import subprocess
import pandas as pd
import sys

DOCKERFILES_PATH = "../dataset/smellfree_dockerfiles"
ORIGINAL_REPOS_PATH = "../dataset/original_repos"
DOCKERFILE_SUMMARY_FILE = "../dataset/chosen_dockerfiles.csv"

#LOG_FOLDER = "./build_logs_origins"
#LOG_FOLDER = "./build_logs_smellfree"

#LOG_FOLDER = "./build_logs_fixed_original"
#LOG_FOLDER = "./build_logs_fixed_smellfree"

#LOG_FOLDER = "./build_logs_fixed"
LOG_FOLDER = "./build_logs_injected"
 
def build_docker_image(context_folder, dockerfile_name):
    with open(LOG_FOLDER + '/' + dockerfile_name + ".log", 'w') as f:
        try:
            cmd = f'docker build -t {context_folder["image_name"]}:dockercleaner -f {dockerfile_name} .'
            f.write(f"--> Building docker image with command: {cmd}\n")
            f.flush()
            res = subprocess.call(cmd, stdout=f, stderr=subprocess.STDOUT, cwd=context_folder["root"], shell=True)
            return res
        except Exception as e:
            print(f"Error while building {dockerfile_name}: ", str(e))
            f.write(f"Error while building {dockerfile_name}: {str(e)}\n")
            f.flush()
            return 1
 
def clean_docker_stuffs(context_folder):
    # clean all docker containers
    subprocess.call("docker rm $(docker ps -aq)", cwd=context_folder["root"], shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)
    # clean the dangling images
    subprocess.call('docker rmi -f $(docker images -f "dangling=true" -q)', cwd=context_folder["root"], shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)
    # clean the built images
    subprocess.call('docker rmi -f $(docker images --filter=reference="*:dockercleaner" -q)', cwd=context_folder["root"], shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)
 
def collect_dockerfile_context_folders(downstream_dockerfiles_path):
    dockerfile_context_folders = []
    df = pd.read_csv(DOCKERFILE_SUMMARY_FILE, sep=';')
    for index, row in df.iterrows():
        image_name = row["Name"].strip()
        link_parts = row["dockerfile_link"].strip().split("/")
        folder_path = link_parts[7:len(link_parts) - 1]
        if len(folder_path) == 0:
            folder_path.append(".")

        upstream_dockerfile_root_path = ORIGINAL_REPOS_PATH + "/" + row["Name"] + "/" + str.join('/', folder_path) + "/"

        if downstream_dockerfiles_path:
            upstream_dockerfile_names = [f for f in os.listdir(downstream_dockerfiles_path) if f.startswith(image_name + "__")]
            for f_name in upstream_dockerfile_names:
                shutil.copy2(downstream_dockerfiles_path + "/" + f_name, upstream_dockerfile_root_path + f_name)
        else:
            if "open-liberty" == row["Name"]:
                upstream_dockerfile_names = ['Dockerfile.ubuntu.openjdk11']
            elif "mysql" == row["Name"]:
                upstream_dockerfile_names = ['Dockerfile.debian']
            elif "websphere-liberty" == row["Name"]:
                upstream_dockerfile_names = ['Dockerfile.ubuntu.openjdk11']
            else:
                upstream_dockerfile_names = ["Dockerfile"]

        dockerfile_context_folders.append({"root": upstream_dockerfile_root_path, "image_name": image_name, "dockerfiles": upstream_dockerfile_names}) 
 
    return dockerfile_context_folders
 
 
 
if __name__ == "__main__":
    all_images = ["adminer","aerospike","api-firewall","arangodb","backdrop","bash","bonita","caddy","cassandra","chronograf","composer","consul","convertigo","couchbase","couchdb","crate","docker","drupal","eclipse-mosquitto","eggdrop","emqx","express-gateway","flink","fluentd","friendica","gazebo","geonetwork","ghost","gradle","haproxy","hello-world","hitch","httpd","influxdb","irssi","jetty","joomla","kapacitor","kong","lightstreamer","mariadb","matomo","maven","mediawiki","memcached","mongo-express","mongo","monica","mysql","nats","neo4j","neurodebian","nextcloud","nginx","notary","odoo","open-liberty","orientdb","percona","php-zendserver","phpmyadmin","plone","postfixadmin","postgres","rabbitmq","rakudo-star","redis","redmine","registry","rethinkdb","rocket.chat","satosa","silverpeas","solr","sonarqube","spiped","storm","teamspeak","telegraf","tomcat","tomee","traefik","varnish","vault","websphere-liberty","wordpress","xwiki","yourls","znc","zookeeper","gcc"]
    selected_images = all_images

    if len(sys.argv) > 1:
        downstream_dockerfiles_path = sys.argv[1]
    else:
        downstream_dockerfiles_path = None
    
    context_folders = collect_dockerfile_context_folders(downstream_dockerfiles_path)
    overall_file = open(LOG_FOLDER + '/' + "overall_res.csv", "a")
    i = 0

    for image in selected_images:
        i += 1
        for folder in context_folders:
            if not folder["image_name"] == image:
                continue
 
            for dockerfile_name in folder["dockerfiles"]:
                print(f"--> Building docker image for: {dockerfile_name}")
                res = build_docker_image(folder, dockerfile_name)
                print(f"Result: {res}")
    
                overall_file.write(f"{dockerfile_name},{res}\n")
                overall_file.flush()
            
            clean_docker_stuffs(folder)
 
    overall_file.close()
