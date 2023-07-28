import json
from os import listdir
import subprocess

FIXING_METADATA_PATH="../dataset/fixing_metadata_binnacle"
SMELLY_DOCKERFILE_PATH="../dataset/smelly_dockerfiles_bianncle"

#FIXING_METADATA_PATH="../fixing_metadata_original"
#SMELLY_DOCKERFILE_PATH="../dataset/original_dockerfiles"

SMELL_LIST = [
    "use-no-install-recommends", "do-not-use-apt-get-update-alone", "pin-package-manager-versions-apt-get", "pin-package-manager-versions-pip",
    "pin-package-manager-versions-npm", "pin-package-manager-versions-gem", "pin-package-manager-versions-apk",
    "use-copy-instead-of-add", "use-wget-instead-of-add", "do-not-have-secrets", "have-a-healthcheck", "have-a-user"
]

def collect_dockerfile_with_smells(smell_dockerfiles_path):
    dockerfiles = [f for f in listdir(smell_dockerfiles_path)]
    dockerfile_metadata_map = {}
    for file_name in dockerfiles:
        dockerfile_metadata_map[file_name] = SMELL_LIST
    return dockerfile_metadata_map

def fix_single_dockerfile(file_path, smells, result_generated_path):
    smell_args = " --smell ".join(smells)
    smell_args = "--smell " + smell_args
    try:
        subprocess.call(f"dockercleaner -i {file_path} -m --fix {smell_args} --modified-date 2023-04-12 > {result_generated_path}",
                        shell=True)
        return True
    except Exception:
        return False
def fix_dockerfiles(dockerfile_metadata_map, dockerfiles_path, output_path):
    dockerfile_metadata_map = dict(sorted(dockerfile_metadata_map.items()))
    start_from = ''
    start = False
    run_dockerfiles = []
    i = 0
    total = len(dockerfile_metadata_map)
    for dockerfile, smells in dockerfile_metadata_map.items():
        i += 1
        if start_from and dockerfile == start_from:
            start = True

        if start_from and start == False:
            continue
        
        if dockerfile not in run_dockerfiles:
            continue

        res = fix_single_dockerfile(dockerfiles_path + "/" + dockerfile,
                            smells,
                            output_path + "/" + dockerfile.replace(".dockerfile", ".json"))
        
        if res:
            print(f"> [{i}/{total}] Fixed Dockerfile: " + dockerfiles_path + "/" + dockerfile)
        else:
            print(f"[{i}/{total}] Error while fixing: " + dockerfiles_path + "/" + dockerfile)

        #if i == 1000:
        #    break

if __name__ == "__main__":
    dockerfile_metadata_map = collect_dockerfile_with_smells(SMELLY_DOCKERFILE_PATH)
    fix_dockerfiles(dockerfile_metadata_map, SMELLY_DOCKERFILE_PATH, FIXING_METADATA_PATH)
