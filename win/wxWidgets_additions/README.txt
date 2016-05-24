There's a few changes that need to be made to the wxWidgets 3.0.2 source
before you build it.  You can choose to apply the patches listed below.

Or you may copy the wxWidgets-3.0.2 subdirectory in this directory to
where your wxWidgets-3.0.2 source tree is located, overlaying the existing
files.

Distributed versions of Audacity are built with accessibility enabled.
This requires a patch to wxWidgets:

   accessibility.diff

In some cases (see bug #1266 and http://reviews.llvm.org/D13647#eeab044e), the
wxWidgets wxRenameFile function can fail.  This patch provides a workaround:

   fix_rename.diff
   
Other patches that need to be applied to wxWidgets 3.0.2.  These should not
be required for later versions:

   changeset_0797a6f8754db982b87a1df63975ccf76df2905f.diff
   changeset_14f052a7680fc5ddb3e95d29c23b79c7209c9353.diff

