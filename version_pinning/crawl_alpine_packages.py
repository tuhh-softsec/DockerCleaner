from pathlib import Path
import gzip
from urllib import request
import codecs
import tarfile
import datetime

utf8reader = codecs.getreader('utf-8')

distros = ["edge","latest-stable","v3.0","v3.1","v3.10","v3.11","v3.12","v3.13","v3.14","v3.15","v3.16","v3.17","v3.2","v3.3","v3.4","v3.5","v3.6","v3.7","v3.8","v3.9"]
versions = ["main", "community"]

def download_package_lists(distro, ver):
    to_save_path = Path("alpine_packages", distro, ver)
    to_save_path.mkdir(parents=True, exist_ok=True)
    request.urlretrieve(f"http://dl-cdn.alpinelinux.org/alpine/{distro}/{ver}/x86_64/APKINDEX.tar.gz", Path(to_save_path, "APKINDEX.tar.gz"))


def parse_package_lists(distro, ver):
    packages = {}
    with tarfile.open(f"alpine_packages/{distro}/{ver}/APKINDEX.tar.gz", encoding="utf-8") as tar:
        f = utf8reader(tar.extractfile("APKINDEX"))

        package_name = ""
        package_version = ""
        for line in f:
            line = line.strip()
            if line.startswith("P:"):
                package_name = line[2:].strip()
                if package_name not in packages:
                    packages[package_name] = []
            if line.startswith("V:"):
                package_version = line[2:].strip()
            if line.startswith("t:"):
                package_date = line[2:]
                package_date = datetime.datetime.fromtimestamp(int(package_date)).date()
                version_json = {"version": package_version, "modified_date": str(package_date)}
                packages[package_name].append(version_json)
                #print(package_name, "-->", version_json)

    return packages

if __name__ == "__main__":
    all_packages = {}
    for dis in distros:
        for ver in versions:
            try:
                print(dis, ver)
                download_package_lists(dis, ver)
                package_list = parse_package_lists(dis, ver)

                if dis not in all_packages:
                    all_packages[dis] = package_list
                else:
                    current_packages = all_packages[dis]
                    for pkg in package_list: # add new versions to the same package
                        if pkg in current_packages:
                            #print(pkg)
                            current_packages[pkg].extend(package_list[pkg])
                        else:
                            current_packages[pkg] = package_list[pkg]
            except Exception as ex:
                print(ex)

    with open("alpine_packages.csv", "w") as f:
        for distro in all_packages.keys():
            for package in all_packages[distro].keys():
                for v_record in all_packages[distro][package]:
                    version = v_record["version"]
                    mdate = v_record["modified_date"]
                    f.write(f"{distro},{package},{version},{mdate}\n")
            f.flush()