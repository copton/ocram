#!/bin/bash

export EC_RANDOM_SEED=$RANDOM
export EC_MAX_TIME=100000
unset EC_LOG_CORE

EC_LOG_FILE=/tmp/tc.log ./tc.exe || exit 1
EC_LOG_FILE=/tmp/ec.log ./ec.exe || exit 1
EC_LOG_CORE=TRUE EC_LOG_FILE=/tmp/native.log ./native.exe || exit 1

diff /tmp/tc.log /tmp/ec.log || exit 1

tmp=`tempfile`
echo $tmp
grep -- "->" /tmp/tc.log > $tmp
diff $tmp /tmp/native.log || exit 1
rm -f $tmp
