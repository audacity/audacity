/**********************************************************************

Sneedacity: A Digital Audio Editor

AutoRecoveryDialog.h

Paul Licameli split from AutoRecovery.h

**********************************************************************/

#ifndef __SNEEDACITY_AUTO_RECOVERY_DIALOG__
#define __SNEEDACITY_AUTO_RECOVERY_DIALOG__

class SneedacityProject;

//
// Show auto recovery dialog if there are projects to recover. Should be
// called once at Sneedacity startup.
//
// This function possibly opens NEW project windows while it recovers all
// projects. If so, it will re-use *pproj, if != NULL and set it to NULL.
//
// Returns: True, if the start of Sneedacity should continue as normal
//          False if Sneedacity should be quit immediately
//
// The didRecoverAnything param is strictly for a return value.
// Any value passed in is ignored.
//
bool ShowAutoRecoveryDialogIfNeeded(SneedacityProject*& pproj,
                                    bool *didRecoverAnything);

#endif
