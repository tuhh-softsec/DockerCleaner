#!/usr/bin/env bash

pwd1=$(pwd)
cd ..
DOCKERFILE_PATH="$(pwd)/dataset/smellfree_dockerfiles"
cd "$pwd1"

MY_SEED="12345"

for dockerfile in ${DOCKERFILE_PATH}/*; do
  dockerfile=${dockerfile##*/}
  for i in {1..10}; do
    echo "Processing $dockerfile ($i / 10)"

    SEED=$(bash -c "RANDOM=$MY_SEED; echo \$RANDOM")
    MY_SEED=$SEED

    RESULT_FILENAME_INJECT=../dataset/smell_metadata/${dockerfile%.dockerfile}__${i}__injected.json

    dockercleaner --seed $SEED -i $DOCKERFILE_PATH/$dockerfile -m --inject --random-smells > $RESULT_FILENAME_INJECT
    echo "> $RESULT_FILENAME_INJECT"
  done
done
