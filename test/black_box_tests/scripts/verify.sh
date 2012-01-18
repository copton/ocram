#!/bin/bash

verify() {
	export EC_RANDOM_SEED=$RANDOM
	export EC_MAX_TIME=100000

	EC_LOG_FILE=/tmp/tc.log ./tc.exe || exit 1
	EC_LOG_FILE=/tmp/ec.log ./ec.exe || exit 1
	EC_LOG_FILE=/tmp/native.log ./native.exe || exit 1

	diff /tmp/native.log /tmp/tc.log || exit 1
	diff /tmp/tc.log /tmp/ec.log || exit 1
}

for app in $ROOT/application/*; do
	if [ -d $app ]; then
		(cd $app; echo "verifying `basename $app`"; verify)
	fi
done
