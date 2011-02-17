#!/bin/bash

exec runhaskell main.hs -I ~/scm/tc-case_study/src -I ~/scm/tc-case_study/src/operating_system/tinyos/tc ~/scm/tc-case_study/src/application/collect_and_forward/tc-based/app.c
