#!/usr/bin/env bash

# Time-stamp: <2010-12-01 11:45:15 Wednesday by taoshanwen>

# @version 1.0
# @author ahei

svn st | `which grep` -F '?' | xargs rm -rf
find '(' -name "*~" -o -name "#*#" ')' -type f | xargs rm -rf
