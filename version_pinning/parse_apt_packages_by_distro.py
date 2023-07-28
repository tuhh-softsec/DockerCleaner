from pathlib import Path
from urllib import request
import gzip
import lzma
import json

OS="u"

if OS == "u":
    distros = ["bionic-security","bionic-updates","bionic","focal-security","focal-updates","focal","jammy-security","jammy-updates","jammy","kinetic-security","kinetic-updates","kinetic","lunar-security","lunar-updates","lunar","trusty-security","trusty-updates","trusty","xenial-security","xenial-updates","xenial"]
    versions = ["main", "multiverse", "restricted", "universe"]
elif OS == "d":
    distros = ["Debian10.13","Debian11.6","bookworm","bullseye","buster", "stretch", "stable","bookworm-updates","bullseye-updates", "buster-updates", "stretch-updates", "stable-updates"]
    versions = ["main"]
else:
    distros = ["bookworm-security","bullseye-security","buster/updates","stable-security","stretch/updates"]
    versions = ["main"]

def download_package_lists(distro, ver):
    if OS == "u":
        to_save_path = Path("ubuntu_packages", distro, ver)
    elif OS == "d":
        to_save_path = Path("debian_packages", distro, ver)
    else:
        to_save_path = Path("debian_security_packages", distro, ver)
    
    to_save_path.mkdir(parents=True, exist_ok=True)
    
    if OS == "u":
        request.urlretrieve(f"http://de.archive.ubuntu.com/ubuntu/dists/{distro}/{ver}/binary-amd64/Packages.gz", Path(to_save_path, "Packages.gz"))
    elif OS == "d":
        request.urlretrieve(f"http://ftp.de.debian.org/debian/dists/{distro}/{ver}/binary-amd64/Packages.xz", Path(to_save_path, "Packages.xz"))
    else:
        request.urlretrieve(f"https://security.debian.org/debian-security/dists/{distro}/{ver}/binary-amd64/Packages.xz", Path(to_save_path, "Packages.xz"))

def parse_package_lists(distro, ver, package_dates):
    packages = {}
    f = gzip.open(f"ubuntu_packages/{distro}/{ver}/Packages.gz", "r") if OS == "u" else (lzma.open(f"debian_packages/{distro}/{ver}/Packages.xz", "r") if OS == "d" else lzma.open(f"debian_security_packages/{distro}/{ver}/Packages.xz", "r"))
    package_name = ""
    package_version = ""
    for line in f:
        line = line.decode("utf-8")
        line = line.strip()
        if line.startswith("Package:"):
            package_name = line.split(": ")[-1].strip()
            if package_name not in packages:
                packages[package_name] = []
        if line.startswith("Version:"):
            package_version = line[8:].strip()

            if package_name in package_dates:
                if package_version in package_dates[package_name]:
                    version_json = {"version": package_version, "modified_date": package_dates[package_name][package_version]}
                    packages[package_name].append(version_json)
                elif ":" in package_version:
                    refined_version = package_version.split(":")[-1]
                    if refined_version in package_dates[package_name]:
                        version_json = {"version": package_version, "modified_date": package_dates[package_name][refined_version]}
                        packages[package_name].append(version_json)
                    else:
                        print(f"{package_name}={refined_version} is not in the date database!")
                else:
                    print(f"{package_name}={package_version} is not in the date database!")
    return packages


if __name__ == "__main__":
    with open("ubuntu_package_dates.json" if OS == "u" else ("debian_package_dates.json" if OS == "d" else "debian_security_package_dates.json"), "r") as f:
        package_dates = json.load(f)

    all_packages = {}
    for dis in distros:
        for ver in versions:
            try:
                print(dis, ver)
                download_package_lists(dis, ver)
                package_list = parse_package_lists(dis, ver, package_dates)
                distro = dis.split("-")[0] if "-" in dis else dis
                if distro not in all_packages:
                    all_packages[distro] = package_list
                else:
                    current_packages = all_packages[distro]
                    for pkg in package_list: # add new versions to the same package
                        if pkg in current_packages:
                            #print(pkg)
                            current_packages[pkg].extend(package_list[pkg])
                        else:
                            current_packages[pkg] = package_list[pkg]
            except Exception as ex:
                print(ex)

    with open("ubuntu_packages.csv" if OS == "u" else ("debian_packages.csv" if OS == "d" else "debian_security_packages.csv"), "w") as f:
        for distro in all_packages.keys():
            for package in all_packages[distro].keys():
                for v_record in all_packages[distro][package]:
                    version = v_record["version"]
                    mdate = v_record["modified_date"]
                    f.write(f"{distro},{package},{version},{mdate}\n")

            f.flush()