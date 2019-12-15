#!/bin/bash
#
# Write the Git commit SHA to an include file.
# This should be run before compiling code on Linux or Macintosh.
#
revision_filename=src/common/pa_gitrevision.h

# Run git first to make sure it is installed before corrupting the
# include file.
git rev-parse HEAD

# Update the include file with the current Git revision.
echo -n "#define PA_GIT_REVISION " > ${revision_filename}
git rev-parse HEAD >> ${revision_filename}

echo ${revision_filename} now contains
cat ${revision_filename}
