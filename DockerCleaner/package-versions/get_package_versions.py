import sys
import subprocess
import csv
import datetime
import sqlite3

def find_versions_csv(package_names, distro_name, modified_date, db_file):
    package_names = package_names.replace(",", "\|")
    try:
        if distro_name == "all":
            filtered_res = subprocess.check_output(f"grep -w '{package_names}' {db_file}", shell=True)
        else:
            filtered_res = subprocess.check_output(f"grep -w '{package_names}' {db_file} | grep {distro_name}", shell=True)
    except:
        print("Not found packages in the package database file")
        return {}

    filtered_res = filtered_res.decode("utf-8")
    reader = csv.reader(filtered_res.strip().split("\n"))
    
    package_versions = {}
    
    for row in reader:
        dis = row[0]
        pkg_name = row[1]
        if (dis == distro_name or distro_name == "all") and pkg_name in package_names:
            if pkg_name not in package_versions:
                package_versions[pkg_name] = [(row[2], row[3])]
            else:
                package_versions[pkg_name].append((row[2], row[3]))

    closet_versions = {}
    modified_date = datetime.date.fromisoformat(modified_date)
    for pkg_name in package_versions:
        package_versions[pkg_name] = sorted(package_versions[pkg_name], key=lambda x: datetime.date.fromisoformat(x[1]), reverse=True)
        i = 0
        while True:
            current_version = package_versions[pkg_name][i]
            current_version_date = datetime.date.fromisoformat(current_version[1])
            if current_version_date <= modified_date:
                closet_versions[pkg_name] = current_version[0]
                break
            i += 1
        
    return closet_versions

def find_versions_sqlite(package_names, distro_name, modified_date, db_file):
    con = sqlite3.connect(db_file)
    cur = con.cursor()
    package_names = "('" + package_names.replace(",", "','") + "')"
    
    if distro_name == "all":
        query = f"SELECT distro, package, version, modified_date FROM Packages WHERE package IN {package_names} ORDER BY modified_date DESC"
    else:
        query = f"SELECT distro, package, version, modified_date FROM Packages WHERE distro ='{distro_name}' and package IN {package_names} ORDER BY modified_date DESC"
    
    cur.execute(query)
    rows = cur.fetchall()
    if len(rows) == 0:
        print("Not found packages in the package database file")
        return {}
    
    package_versions = {}
    
    for row in rows:
        dis = row[0]
        pkg_name = row[1]
        if (dis == distro_name or distro_name == "all") and pkg_name in package_names:
            if pkg_name not in package_versions:
                package_versions[pkg_name] = [(row[2], row[3])]
            else:
                package_versions[pkg_name].append((row[2], row[3]))
    
    closet_versions = {}
  
    modified_date = datetime.date.fromisoformat(modified_date)
    for pkg_name in package_versions:
        i = 0
        while True:
            print(pkg_name)
            current_version = package_versions[pkg_name][i]
            current_version_date = datetime.date.fromisoformat(current_version[1])
            if current_version_date <= modified_date:
                closet_versions[pkg_name] = current_version[0]
                break
            i += 1

    return closet_versions

if __name__ == "__main__":
    args = sys.argv[1:]

    if len(args) < 4:
        with open("package-versions.csv", "w") as f:
            f.write("package,version\n")
        exit(0)

    distro_name = args[0]
    modified_date = args[1]
    db_file = args[2]
    package_names = args[3]

    if "apt" in db_file and distro_name == "latest":
        distro_name = "lunar"

    if "apk" in db_file and distro_name == "latest":
        distro_name = "v3.17"

    if "apt" in db_file or "apk" in db_file:
        db_file = db_file.replace(".csv", ".db")
        closet_versions = find_versions_sqlite(package_names, distro_name, modified_date, db_file)
    else:
        closet_versions = find_versions_csv(package_names, distro_name, modified_date, db_file)

    with open("package-versions.csv", "w") as f:
        f.write("package,version\n")
        for k, v in closet_versions.items():
            f.write(f"{k},{v}\n")
