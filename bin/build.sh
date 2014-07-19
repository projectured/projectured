#!/bin/sh

. `dirname "$0"`/environment.sh
. ${DWIM_WORKSPACE}/hu.dwim.build/bin/build-production-executable.sh
tar -cvzf ${DWIM_PROJECT_NAME}.tar.gz ${DWIM_PROJECT_NAME} image font
