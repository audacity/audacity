/**********************************************************************

  Audacity: A Digital Audio Editor

  ProjectFSCK.h

  A function that performs consistency checks on the tree of block files

  Paul Licameli split this out of DirManager.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_FSCK__
#define __AUDACITY_PROJECT_FSCK__

class DirManager;

enum : unsigned {
    FSCKstatus_CLOSE_REQ = 0x1,
    FSCKstatus_CHANGED   = 0x2,
    FSCKstatus_SAVE_AUP  = 0x4,// used in combination with FSCKstatus_CHANGED
};

// Check the project for errors and possibly prompt user
// bForceError: Always show log error alert even if no errors are found here.
//    Important when you know that there are already errors in the log.
// bAutoRecoverMode: Do not show any option dialogs for how to deal with errors found here.
//    Too complicated during auto-recover. Just correct problems the "safest" way.
int ProjectFSCK(
    DirManager& dm, const bool bForceError, const bool bAutoRecoverMode);

#endif
