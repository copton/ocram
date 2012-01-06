#!/bin/bash

export EC_RANDOM_SEED=$RANDOM
export EC_MAX_TIME=100000

#EC_LOG_FILE=/tmp/tc.log ./tc.exe || exit 1
EC_LOG_FILE=/tmp/ec.log ./ec.exe || exit 1
EC_LOG_FILE=/tmp/native.log ./native.exe || exit 1

#diff /tmp/tc.log /tmp/native.log || exit 1
#diff /tmp/tc.log /tmp/ec.log || exit 1
diff /tmp/native.log /tmp/ec.log || exit 1
