#!/bin/bash
#
# Clear the SVN revision in the include file.
# This should be run before checking in code to SVN.
#
revision_filename=src/common/pa_svnrevision.h

# Update the include file with the current SVN revision.
echo "#define PA_SVN_REVISION unknown" > ${revision_filename}

echo ${revision_filename} now contains
cat ${revision_filename}
