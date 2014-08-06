#!/bin/sh

. `dirname "$0"`/environment.sh
. ${DWIM_WORKSPACE}/hu.dwim.build/bin/build-production-executable.sh
tar -C ${DWIM_WORKSPACE} -cvzf ${DWIM_PROJECT_NAME}.tar.gz ${DWIM_PROJECT_NAME}/${DWIM_PROJECT_NAME} ${DWIM_PROJECT_NAME}/image ${DWIM_PROJECT_NAME}/font ${DWIM_PROJECT_NAME}/example
