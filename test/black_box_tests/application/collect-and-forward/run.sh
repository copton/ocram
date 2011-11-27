#!/bin/bash

export EC_RANDOM_SEED=$RANDOM
export EC_MAX_TIME=1000000

EC_LOG_FILE=/tmp/tc.log ./tc.elf
EC_LOG_FILE=/tmp/ec.log ./ec.elf

diff /tmp/tc.log /tmp/ec.log
