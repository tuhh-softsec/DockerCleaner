from os import listdir
from os.path import isfile, join
from pathlib import Path
import shutil
import sys
import pandas as pd
import subprocess
import glob
import json
import os
from pandas.errors import EmptyDataError

SMELL_LIST = [
    "use-no-install-recommends", "do-not-use-apt-get-update-alone", "pin-package-manager-versions-apt-get", "pin-package-manager-versions-pip",
    "pin-package-manager-versions-npm", "pin-package-manager-versions-gem", "pin-package-manager-versions-apk",
    "use-copy-instead-of-add", "use-wget-instead-of-add", "do-not-have-secrets", "have-a-healthcheck", "have-a-user"
]

KICS_SMELL_LIST = {
    "9bae49be-0aa3-4de5-bab2-4c3a069e40cd": "do-not-use-apt-get-update-alone",
    "4b410d24-1cbe-4430-a632-62c9a931cf1c": "use-wget-instead-of-add",
    "3e2d3b2f-c22a-4df1-9cc6-a7a0aebb0c99": "do-not-have-secrets",
    "baee238e-1921-4801-9c3f-79ae1d7b2cbc": "do-not-have-secrets",
    "487f4be7-3fd9-4506-a07a-eae252180c08": "do-not-have-secrets",
    "76c0bcde-903d-456e-ac13-e58c34987852": "do-not-have-secrets",
    "83ab47ff-381d-48cd-bac5-fb32222f54af": "do-not-have-secrets",
    "e9856348-4069-4ac0-bd91-415f6a7b84a4": "do-not-have-secrets",
}

HADOLINT_SMELL_LIST = {
    "DL3015": "use-no-install-recommends",
    "DL3008": "pin-package-manager-versions-apt-get",
    "DL3013": "pin-package-manager-versions-pip",
    "DL3016": "pin-package-manager-versions-npm",
    "DL3028": "pin-package-manager-versions-gem",
    "DL3018": "pin-package-manager-versions-apk",
    "DL3006": "pin-base-image-version",
    "DL3020": "use-copy-instead-of-add",
    "DL3057": "have-a-healthcheck",
    "DL3002": "have-a-user"
}

SMELL_METADATA_PATH="../dataset/smell_metadata"
FIXING_METADATA_PATH="../dataset/fixing_metadata"
SMELLY_DOCKERFILE_PATH="../dataset/smelly_dockerfiles"
FIXED_DOCKERFILE_PATH="../dataset/fixed_dockerfiles"

def process_metadatas(metadata_path, metadata_files, generated_dockerfile_path):
    print("Processing metadata...")
    df = pd.DataFrame(columns=["file_name", "try_injected_smells", "injected_smells", "try_fixed_smells", "fixed_smells"])

    for file_name in metadata_files:
        with open(metadata_path + "/" + file_name, "r") as f:
            try:
                data = json.load(f)
            except:
                print("Broken json file: " + file_name)
    
        processed_dockerfile = data["processedDockerfile"]
        dockerfile_path = generated_dockerfile_path + "/" + file_name.replace(".json", ".dockerfile")
        with open(dockerfile_path, "w") as f:
            f.writelines(processed_dockerfile)

        try_injected_smells = data["injectedSmells"]
        injected_smells = data["successfullyInjectedSmells"]
        to_fixed_smells = data["fixedSmells"]
        fixed_smells = data["successfullyFixedSmells"]
        df = pd.concat(
            [df, 
            pd.DataFrame.from_records(
                {
                    "file_name": os.path.basename(dockerfile_path), 
                    "try_injected_smells": ','.join(try_injected_smells), 
                    "injected_smells": ','.join(injected_smells), 
                    "try_fixed_smells": ','.join(to_fixed_smells), 
                    "fixed_smells": ','.join(fixed_smells)
                },
                index=[0]
            )], ignore_index=True
        )
    df.to_csv("./dockercleaner_results.csv", index=False)
    print("Done!")

def collect_oracle_results(dockerfile_path):
    print("Running Hadolint...")
    subprocess.call(f"./runHadolint.sh {dockerfile_path}", shell=True)
    print("Done!")

    # docker service has to be started before running Kics
    print("Running Kics...")
    subprocess.call(f"./runKics.sh {dockerfile_path}", shell=True)
    print("Done!")

    hado_rule_ids = list(HADOLINT_SMELL_LIST.keys())
    kics_rule_ids = list(KICS_SMELL_LIST.keys())


    hado_df = pd.read_csv("hadolint_results.csv", header=None)
    hado_df = hado_df[[0, 3]]
    hado_df = hado_df[hado_df[0].isin(hado_rule_ids)]
    hado_df.replace(HADOLINT_SMELL_LIST, inplace=True)
    hado_df[3] = hado_df[3].apply(lambda p: os.path.basename(p))
    hado_df.columns = ["detected_smells", "file_name"]

    try:
        kics_df = pd.read_csv("kics_results.csv", header=None)
        kics_df = kics_df[[0, 5]]
        kics_df = kics_df[kics_df[0].isin(kics_rule_ids)]
        kics_df.replace(KICS_SMELL_LIST, inplace=True)
        kics_df[5] = kics_df[5].apply(lambda p: os.path.basename(p))
        kics_df.columns = ["detected_smells", "file_name"]
        oracle_df = pd.concat([hado_df, kics_df], ignore_index=True)
    except EmptyDataError:
        print("Kics found no smells!")
        oracle_df = hado_df

    oracle_df = oracle_df.groupby("file_name").agg({"detected_smells": lambda r: ','.join(r)})
    oracle_df.to_csv("./oracle_results.csv")


def combine_results(oracle_df, dockercleaner_df):
    evaluation_df = pd.merge(dockercleaner_df, oracle_df, how="left", on="file_name")
    
    injected_not_detected = []
    detected_not_injected = []
    
    fixed_and_detected = []

    if not pd.isnull(evaluation_df.loc[0, 'injected_smells']):
        for index, row in evaluation_df.iterrows():
            detected_smells = set() if pd.isnull(row['detected_smells']) else set(row['detected_smells'].split(","))

            injected_smells = set() if pd.isnull(row['injected_smells']) else set(row['injected_smells'].split(","))
            injected_not_detected.append(",".join(injected_smells.difference(detected_smells)))
            detected_not_injected.append(",".join(detected_smells.difference(injected_smells)))

        evaluation_df["injected_not_detected"] = injected_not_detected
        evaluation_df["detected_not_injected"] = detected_not_injected
        print(injected_not_detected)
        print(detected_not_injected)

    else:
        for index, row in evaluation_df.iterrows():
            detected_smells = set() if pd.isnull(row['detected_smells']) else set(row['detected_smells'].split(","))

            fixed_smells = set() if pd.isnull(row['fixed_smells']) else set(row['fixed_smells'].split(","))
            fixed_and_detected.append(",".join(fixed_smells.intersection(detected_smells)))

        evaluation_df["fixed_and_detected"] = fixed_and_detected
        print(fixed_and_detected)

    evaluation_df.to_csv("./evaluation_results.csv")

def evaluate(metadata_path, dockerfile_path):
    metadata_files = [f for f in listdir(metadata_path) if f.endswith(".json")]
        
    process_metadatas(metadata_path, metadata_files, dockerfile_path)
    collect_oracle_results(dockerfile_path)

    oracle_df = pd.read_csv("./oracle_results.csv")
    dockercleaner_df = pd.read_csv("./dockercleaner_results.csv")
    combine_results(oracle_df, dockercleaner_df)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Add argument 'injection' or 'fixing' or 'originals' to evaluate the results of DockerCleaner")
        exit(1)

    if sys.argv[1] == "injection":
        evaluate(SMELL_METADATA_PATH, SMELLY_DOCKERFILE_PATH)
    elif sys.argv[1] == "fixing":
        evaluate(FIXING_METADATA_PATH, FIXED_DOCKERFILE_PATH)