#!/bin/sh

. `dirname "$0"`/environment.sh
. ${DWIM_WORKSPACE}/hu.dwim.build/bin/build-production-executable.sh
gzexe ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/${DWIM_PROJECT_NAME}
rm ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/${DWIM_PROJECT_NAME}~
tar -C ${DWIM_WORKSPACE} -cvzf ${DWIM_PROJECT_NAME}.tar.gz ${DWIM_PROJECT_NAME}/${DWIM_PROJECT_NAME} ${DWIM_PROJECT_NAME}/font
rm ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/${DWIM_PROJECT_NAME}.build-log
