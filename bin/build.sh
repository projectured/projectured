#!/bin/sh
#| -*- mode: lisp; coding: utf-8-unix -*-

# FIXME this script assumes SBCL at multiple places, but
# ideally it should use cl-launch to abstract away the underlying lisp.

SCRIPT_DIR=`dirname "$0"`
SCRIPT_DIR=`readlink -f ${SCRIPT_DIR}`

cd "${SCRIPT_DIR}"

###
### beginning of settings
###
PROJECT_NAME=ProjecturEd

MAXIMUM_HEAP_SIZE=${MAXIMUM_HEAP_SIZE:-1500}
export EXECUTABLE_CORE_FILE=`readlink -f projectured`
BUILD_LOG_FILE="${EXECUTABLE_CORE_FILE}.build-log"
LISP=${LISP:-sbcl}

# alternatively
#LISP=${SCRIPT_DIR}/../../sbcl/run-sbcl.sh
#LISP=`readlink -f ${LISP}`

###
### end of settings
###

echo "*** "`date`" Building '${PROJECT_NAME}' using lisp '${LISP}'"

# we rely on the user's config, but we could do this, too
#export CL_SOURCE_REGISTRY="(:source-registry (:also-exclude \"sbcl\" \"disabled-systems\") (:tree \"${WORKSPACE}\") :ignore-inherited-configuration)"
#export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (\"${WORKSPACE}\" (\"${INSTALL_PATH}/.cache/common-lisp/\" :implementation)) :ignore-inherited-configuration)"

# we need to delete the exe here, because i don't know how to convince ASDF:PROGRAM-OP to overwrite the output.
# a suggested alternative: (defmethod asdf:perform :before ((op asdf:program-op) (sys (eql (asdf:find-system :my-system)))) (uiop:delete-file-if-exists (asdf:output-file op sys)))
# another one, supposedly better: (defmethod asdf/plan:traverse-action :before (plan (op asdf:program-op) (sys (eql (asdf:find-system :system))) niip) (uiop:delete-file-if-exists (asdf:output-file op sys)))
rm "${EXECUTABLE_CORE_FILE}" "${BUILD_LOG_FILE}" 2>/dev/null

# "call" the lisp part below
exec ${LISP} --dynamic-space-size "${MAXIMUM_HEAP_SIZE}" --noinform --end-runtime-options \
  --eval "(require :asdf)" --eval "(asdf:load-system :asdf)" \
  --eval "(with-open-file (s \"${0}\" :element-type 'character) (read-line s) (load s))" \
  --end-toplevel-options 2>&1 | tee ${BUILD_LOG_FILE}

chmod ug+x "${EXECUTABLE_CORE_FILE}"

echo "*** "`date`" Finished building ${PROJECT_NAME}, executable should be at ${EXECUTABLE_CORE_FILE}"

# let's quit the shell part before the shell interpreter runs on the lisp stuff below
kill -INT $$

# and from here follows the lisp part that gets invoked by the above shell part |#

(defpackage :projectured.build
  (:use :common-lisp))

(in-package :projectured.build)

(format t "~&Running on ~A ~A, using Quicklisp dist version ~A~%"
        (lisp-implementation-type)
        (lisp-implementation-version)
        (or #+quicklisp (ql:dist-version "quicklisp")
            "n/a"))

#-quicklisp
(warn "Quicklisp is not available to download dependencies.")

;; KLUDGE for a quicklisp bug: it doesn't download :defsystem-depends-on dependencies,
;; so we need to explicitly quickload it early on, before the project .asd's get loaded.
;; for more details, see: https://github.com/quicklisp/quicklisp-client/pull/122
#+quicklisp
(ql:quickload :hu.dwim.asdf)

(defconstant +project-system-name+ :projectured.executable)

(defmethod asdf:output-files ((o asdf:program-op) (s (eql (asdf:find-system +project-system-name+))))
  (let ((exe-path (uiop:getenv "EXECUTABLE_CORE_FILE")))
    (assert exe-path)
    (values (list exe-path) t)))

(defun make-all-loaded-asdf-systems-immutable ()
  (let ((loaded-systems/name (asdf:already-loaded-systems)))
    ;; (format t "~%Making the following ASDF systems immutable:~%~A~%~%" loaded-systems/name)
    (mapcar 'asdf:register-immutable-system
            loaded-systems/name))
  (values))

(pushnew 'make-all-loaded-asdf-systems-immutable uiop:*image-dump-hook*)

#+quicklisp
(ql:quickload +project-system-name+)

(asdf:operate 'asdf:program-op +project-system-name+)

;; this is dead man's land here on implementations like SBCL that can only SAVE-LISP-AND-DIE where the die part is not optional.

(format *error-output* "~%*** Something went wrong, SBCL should not return from asdf:program-op...~%~%")
