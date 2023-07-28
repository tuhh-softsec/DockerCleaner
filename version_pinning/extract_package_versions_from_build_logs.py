from os import listdir
from pprint import pprint
import sys

BUILDLOGS_PATH = "../evaluation/build_logs_origins"

def extract_package_versions(image_name, packages):
    build_log_file = f"{BUILDLOGS_PATH}/{image_name}.log"
    with open(build_log_file, "r") as file:
        log_lines = [line.rstrip() for line in file]

    for line in log_lines:
        if "is already the newest version" in line: # apt ca-certificates is already the newest version (20211016ubuntu0.22.04.1).
            line = line.replace("is already the newest version", "")
            extract_package_version_in_line(packages, line)
        elif line.startswith("Unpacking"): # apt Unpacking libjpeg62-turbo-dev:amd64 (1:1.5.2-2+deb10u1) ...
            line = " ".join(line.split()[1:3])
            extract_package_version_in_line(packages, line)
        elif line.startswith("Executing") and line.endswith(".trigger"): #apk Executing ca-certificates-20220614-r0.trigger
            for idx, pkg in enumerate(packages):
                if "=" not in pkg and pkg in line:
                    line = line.replace("Executing ", "").replace(".trigger", "")
                    if line.startswith(pkg):
                        version = line.replace(pkg, "")[1:]
                        packages[idx] = pkg + "=" + version
        else:
            parts = line.split()
            # (22/54) Installing libgomp (12.2.1_git20220924-r4)
            # (6/54) Installing perl-git (2.38.4-r1)
            # (20/41) Upgrading libcrypto1.1 (1.1.1o-r0 -> 1.1.1t-r2)
            if len(parts) >= 4 and (parts[1] == "Installing" or parts[1] == "Upgrading"): # apk
                extract_package_version_in_line(packages, " ".join(parts[2:]))

def extract_package_version_in_line(packages, line):
    parts = line.split()
    package_part = parts[0]
    version_part = parts[1]

    if len(parts) >= 4 and parts[2] == "->":
        version_part = " ".join(parts[1:])

    if ":amd64" in package_part:
        package_part = package_part.replace(":amd64", "")

    for idx, pkg in enumerate(packages):
        if "=" not in pkg and pkg == package_part:
            if "->" in version_part:
                ver_start = version_part.index("->") + 3
            else:
                ver_start = version_part.index("(") + 1
            ver_end = version_part.index(")", ver_start)
            version = version_part[ver_start: ver_end]
            packages[idx] = pkg + "=" + version

if __name__ == "__main__":
    image_name = sys.argv[1]
    package_names = sys.argv[2]

    alias_packages = {
        "ninja": "samurai",
        "postgresql-dev": "postgresql14-dev",
        "llvm-dev": "llvm12-dev",
        "openssh-client": "openssh-client-default",
        "man": "man-db",
        "libc-dev": "libc6-dev"
    }

    packages = package_names.split(",")
    extract_package_versions(image_name, packages)

    with open("package-versions.csv", "w") as f:
        f.write("package,version\n")
        for pkg in packages:
            if "=" in pkg:
                pkg_name = pkg.split("=")[0]
                pkg_ver = pkg.split("=")[1]
                f.write(f"{pkg_name},{pkg_ver}\n")
            elif pkg in alias_packages:
                pkg_list = [alias_packages[pkg]]
                extract_package_versions(image_name, pkg_list)
                pkg_res = pkg_list[0]
                if "=" in pkg_res:
                    pkg_ver = pkg_res.split("=")[1]
                    f.write(f"{pkg},{pkg_ver}\n")
                else:
                    f.write(f"{pkg},\n")
            else:
                f.write(f"{pkg},\n")