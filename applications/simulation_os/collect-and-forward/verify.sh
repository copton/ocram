#!/bin/bash

export EC_RANDOM_SEED=$RANDOM
export EC_MAX_TIME=100000

EC_LOG_FILE=/tmp/tc.log ./tc.elf || exit 1
EC_LOG_FILE=/tmp/ec.log ./ec.elf || exit 1
EC_LOG_FILE=/tmp/native.log ./native.elf || exit 1

diff /tmp/native.log /tmp/tc.log || exit 1
diff /tmp/tc.log /tmp/ec.log || exit 1
