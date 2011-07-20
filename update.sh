#!/bin/bash

# Time-stamp: <10/30/2009 15:52:56 星期五 by ahei>

bin=`dirname "$0"`
bin=`cd "$bin"; pwd`

svn up "${bin}" "$@"
"${bin}"/install.emacs.sh
