#!/bin/sh

export DWIM_PROJECT_NAME="projectured"
export DWIM_SWANK_PORT="13000"
export DWIM_MAXIMUM_MEMORY_SIZE="1500Mi"
export DWIM_TOPLEVEL_FUNCTION="projectured::executable-toplevel"
export DWIM_ASDF_SYSTEM_NAME="projectured.product"
export DWIM_WORKSPACE="/home/levy/workspace"
export DWIM_PROJECT_PATH="${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}"
export DWIM_INSTALL_PATH="${DWIM_PROJECT_PATH}"
export DWIM_EXECUTABLE_CORE_FILE="${DWIM_INSTALL_PATH}/${DWIM_PROJECT_NAME}"
