#!/usr/bin/env bash

cat <<-EOH
# This file is generated via https://github.com/Silverpeas/docker-silverpeas-prod/blob/master/generate-docker-library.sh
Maintainers: Miguel Moquillon <miguel.moquillon@silverpeas.org> (@mmoqui)
GitRepo: https://github.com/Silverpeas/docker-silverpeas-prod.git
EOH

function printVersion() {
  cat <<-EOE

Tags: $1
GitCommit: $2
GitFetch: refs/heads/$3
	EOE
}

isFirst=1
currentBase=""
count=0
for version in `git tag | tac | grep "^[0-9.]\+$"`; do
  base=`echo $version | grep -o "[0-9].[0-9]"`
  if [ "$base" != "$currentBase" ]; then
    currentBase="$base"
  else
    continue
  fi

  test $count -eq 2 && break

  count=$(( count + 1 ))
  commit=`git rev-parse ${version}`
  fetch=`echo ${version} | grep -o "[0-9].[0-9]"`.x
  if [ $isFirst -eq 1 ]; then
    isFirst=0
    printVersion "${version}, latest" ${commit} ${fetch}
  else
    printVersion "${version}" ${commit} ${fetch}
  fi
done
