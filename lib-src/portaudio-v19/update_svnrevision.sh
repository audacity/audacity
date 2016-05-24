#!/bin/bash
#
# Write the SVN revision to an include file.
# This should be run before compiling code on Linux or Macintosh.
#
revision_filename=src/common/pa_svnrevision.h

# Run svnversion first to make sure it is installed before corrupting the
# include file.
svnversion .

# Update the include file with the current SVN revision.
echo -n "#define PA_SVN_REVISION " > ${revision_filename}
svnversion . >> ${revision_filename}

echo ${revision_filename} now contains
cat ${revision_filename}
