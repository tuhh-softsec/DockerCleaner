import json
import csv
import sys

if __name__ == "__main__":
    if len(sys.argv) > 1:
        result_file = sys.argv[1]
    else:
        result_file = "kics_results.csv"

    rules = {}
    with open('kics_rules.json') as json_file:
        data = json.load(json_file)
        for i in range(len(data)):
            r = data[i]
            rule = {"id": r[0], "name": r[1], "desc": r[2], "level": r[3]}
            rules[r[0]] = rule

    results = []
    with open(result_file) as csv_file:
        reader = csv.reader(csv_file)
        results = list(reader)

    for result in results:
        if result[0] in rules:
            rule = rules[result[0]]
        else:
            rule = {"id": result[0], "name": "Unknown", "desc": "Unknown", "level": "noinfo"}

        result.insert(1, rule["name"])
        result.insert(2, rule["desc"])
        result.insert(3, rule["level"])

    with open(result_file, 'w') as csv_file:
        writer = csv.writer(csv_file)
        writer.writerows(results)