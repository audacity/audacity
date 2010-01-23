/**********************************************************************

  Audacity: A Digital Audio Editor
  
  Dependencies.h
 
  Dominic Mazzoni

  The primary function provided in this source file is
  ShowDependencyDialogIfNeeded.  It checks a project to see if
  any of its WaveTracks contain AliasBlockFiles; if so it
  presents a dialog to the user and lets them copy those block
  files into the project, making it self-contained.

**********************************************************************/

#ifndef __AUDACITY_DEPENDENCIES__
#define __AUDACITY_DEPENDENCIES__

#include <wx/defs.h>
#include <wx/filename.h>

#include "Track.h"

class AliasedFile {
 public:
  AliasedFile(wxFileName fileName, wxLongLong bytes) {
    this->fileName = fileName;
    this->bytes = bytes;
  }
  wxFileName  fileName;
  wxLongLong  bytes; // if stored as current default sample format
};

class AudacityProject;

WX_DECLARE_OBJARRAY(AliasedFile, AliasedFileArray);

// True = success, whether dependencies were found or not
bool FindDependencies(AudacityProject *project,
		      AliasedFileArray *aliasedFiles);

// Checks for alias block files, modifies the project if the
// user requests it, and returns True if the user continues.
// Returns false if the user clicks Cancel, meaning that they do
// not want to go ahead with the Save/Save As operation.
bool ShowDependencyDialogIfNeeded(AudacityProject *project,
                                  bool isSaving);

#endif

// Indentation settings for Vim and Emacs.
// Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

