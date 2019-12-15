#!/bin/bash
#
# Clear the Git commit SHA in the include file.
# This should be run before checking in code to Git.
#
revision_filename=src/common/pa_gitrevision.h

# Update the include file with the current GIT revision.
echo "#define PA_GIT_REVISION unknown" > ${revision_filename}

echo ${revision_filename} now contains
cat ${revision_filename}
