#!/bin/bash

exec runhaskell tc2ec.hs -I ~/scm/tc-case_study/src -I ~/scm/tc-case_study/src/operating_system/tinyos/tc ~/scm/tc-case_study/src/application/collect_and_forward/tc-based/main.c
