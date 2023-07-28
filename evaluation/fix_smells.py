import json
from os import listdir
import subprocess

SMELL_METADATA_PATH="../dataset/smell_metadata"
FIXING_METADATA_PATH="../dataset/fixing_metadata"
SMELLY_DOCKERFILE_PATH="../dataset/smelly_dockerfiles"
FIXED_DOCKERFILE_PATH="../dataset/fixed_dockerfiles"

def collect_dockerfile_with_smells(smell_metadata_path):
    metadata_files = [f for f in listdir(smell_metadata_path) if f.endswith(".json")]
    dockerfile_metadata_map = {}
    for file_name in metadata_files:
        with open(smell_metadata_path + "/" + file_name, "r") as f:
            metadata = json.load(f)
            dockerfile_name = file_name.replace(".json", ".dockerfile")
            dockerfile_metadata_map[dockerfile_name] = metadata["successfullyInjectedSmells"]
    return dockerfile_metadata_map

def fix_single_dockerfile(file_path, smells, result_generated_path):
    if len(smells) == 0:
        print("- No smells to fix for Dockerfile: " + file_path)
    else:
        smell_args = " --smell ".join(smells)
        smell_args = "--smell " + smell_args
        #print(f"dockercleaner -i {file_path} -m --fix {smell_args} > {result_generated_path}")
        subprocess.call(f"dockercleaner -i {file_path} -m --fix {smell_args} --modified-date 2023-04-12 > {result_generated_path}",
                        shell=True)
        print("> Fixed Dockerfile: " + file_path)

def fix_dockerfiles(dockerfile_metadata_map, dockerfiles_path, output_path):
    i = 0
    for dockerfile, smells in dockerfile_metadata_map.items():
        fix_single_dockerfile(dockerfiles_path + "/" + dockerfile,
                              smells,
                              output_path + "/" + dockerfile.replace(".dockerfile", "__fixed.json"))
        i += 1
        #if i >= 100:
            #break

if __name__ == "__main__":
    dockerfile_metadata_map = collect_dockerfile_with_smells(SMELL_METADATA_PATH)
    fix_dockerfiles(dockerfile_metadata_map, SMELLY_DOCKERFILE_PATH, FIXING_METADATA_PATH)