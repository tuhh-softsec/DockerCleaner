#!/usr/bin/env bash

DOCKERFILE_PATH=$1

rm -f hadolint_results.csv

for dockerfile in $(find $DOCKERFILE_PATH -name "*.[Dd]ockerfile"); do
  dockerfilename=${dockerfile##*/}
  hadolint -f sarif --warning=DL3057 $dockerfile | jq '.runs[0].results | map(.locations[0].physicalLocation as $loc | [.ruleId, .message.text, .level, $loc.artifactLocation.uri, $loc.region.sourceLanguage, $loc.region.startLine, $loc.region.endLine, $loc.region.startColumn, $loc.region.endColumn] | @csv) | .[]' -r >> hadolint_results.csv
done
