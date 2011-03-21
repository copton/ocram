#!/bin/bash

exec runhaskell main.hs -c "-I ~/scm/tc-case_study/src -I ~/scm/tc-case_study/src/operating_system/tinyos/tc" -i ~/scm/tc-case_study/src/application/collect_and_forward/tc-based/app.c -a api.h
