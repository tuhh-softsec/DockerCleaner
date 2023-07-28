#!/usr/bin/env bash

DOCKERFILE_PATH=$1

docker run --rm -it -v "$DOCKERFILE_PATH":/path checkmarx/kics:latest scan --include-queries 9bae49be-0aa3-4de5-bab2-4c3a069e40cd,4b410d24-1cbe-4430-a632-62c9a931cf1c,3e2d3b2f-c22a-4df1-9cc6-a7a0aebb0c99,baee238e-1921-4801-9c3f-79ae1d7b2cbc,487f4be7-3fd9-4506-a07a-eae252180c08,76c0bcde-903d-456e-ac13-e58c34987852,83ab47ff-381d-48cd-bac5-fb32222f54af,e9856348-4069-4ac0-bd91-415f6a7b84a4 -p /path --report-formats sarif -o "/path/"

cat $DOCKERFILE_PATH/results.sarif | jq '.runs[0].results | map(.locations[0].physicalLocation as $loc | [.ruleId, .message.text, $loc.artifactLocation.uri, $loc.region.sourceLanguage, $loc.region.startLine, $loc.region.endLine, $loc.region.startColumn, $loc.region.endColumn] | @csv) | .[]' -r > kics_results.csv
python3 update_kics_results.py

rm $DOCKERFILE_PATH/results.sarif