/**********************************************************************

Audacity: A Digital Audio Editor

AutoRecoveryDialog.h

Paul Licameli split from AutoRecovery.h

**********************************************************************/

#ifndef __AUDACITY_AUTO_RECOVERY_DIALOG__
#define __AUDACITY_AUTO_RECOVERY_DIALOG__

class AudacityProject;

//
// Show auto recovery dialog if there are projects to recover. Should be
// called once at Audacity startup.
//
// This function possibly opens NEW project windows while it recovers all
// projects. If so, it will re-use *pproj, if != NULL and set it to NULL.
//
// Returns: True, if the start of Audacity should continue as normal
//          False if Audacity should be quit immediately
//
// The didRecoverAnything param is strictly for a return value.
// Any value passed in is ignored.
//
bool ShowAutoRecoveryDialogIfNeeded(AudacityProject*& pproj, bool* didRecoverAnything);

#endif
