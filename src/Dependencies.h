/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2008 Audacity Team.
   License: GPL v2.  See License.txt.
  
   Dependencies.h

   Dominic Mazzoni
   Vaughan Johnson

**********************************************************************/

#ifndef __AUDACITY_DEPENDENCIES__
#define __AUDACITY_DEPENDENCIES__

class AudacityProject;

// Checks for alias block files, modifies the project if the
// user requests it, and returns True if the user continues.
// Returns false if the user clicks Cancel, meaning that they do
// not want to go ahead with the Save/Save As operation.
bool ShowDependencyDialogIfNeeded(AudacityProject *project,
                                  bool isSaving);

#endif
