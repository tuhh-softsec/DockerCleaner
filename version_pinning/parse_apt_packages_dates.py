import datetime
import json

DATE_FORMAT = '%Y-%m-%d %H:%M'

with open("debian_security_package_dates.csv", "r") as f:
    lines = [line.rstrip() for line in f]

packages = {}
i = 1
prev_date = datetime.date.fromisoformat("1900-01-01")
for line in lines:
    if "->" in line: # symbolic links
        continue

    parts = line.split(",")
    package_file = parts[0]
    package_parts = package_file.split("_")
    package_name = package_parts[0]
    package_version = package_parts[1]

    date = parts[1]

    if package_name not in packages:
        packages[package_name] = {}

    date = datetime.datetime.strptime(date, DATE_FORMAT).date()



    packages[package_name][package_version] = str(date)
    print(package_name, "-->", package_version, "-->", str(date))
    i += 1
    # if i > 1000:
    #     break

with open("debian_security_package_dates.json", "w") as f:
    f.write(json.dumps(packages, indent=2))