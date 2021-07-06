/**********************************************************************

   Sneedacity: A Digital Audio Editor
   Sneedacity(R) is copyright (c) 1999-2008 Sneedacity Team.
   License: GPL v2.  See License.txt.

   Dependencies.h

   Dominic Mazzoni
   Vaughan Johnson

**********************************************************************/

#ifndef __SNEEDACITY_DEPENDENCIES__
#define __SNEEDACITY_DEPENDENCIES__

#include <list>
#include "MemoryX.h"
#include "wxFileNameWrapper.h" // member variable

class SneedacityProject;

class AliasedFile
{
public:
   AliasedFile() {}
   AliasedFile(wxFileNameWrapper &&fileName,
               wxLongLong byteCount, bool bOriginalExists)
      : mFileName(std::move(fileName))
      , mByteCount(byteCount)
      , mbOriginalExists(bOriginalExists)
   {
   }
   AliasedFile(const AliasedFile &that) = default;
   AliasedFile &operator= (AliasedFile&& that)
   {
      if(this != &that) {
         mFileName = std::move(that.mFileName);
         mByteCount = that.mByteCount;
         mbOriginalExists = that.mbOriginalExists;
      }
      return *this;
   }

   wxFileNameWrapper mFileName;
   wxLongLong  mByteCount{}; // if stored as current default sample format
   bool        mbOriginalExists{};
};

// use list, not vector, because we need to take addresses of items in the container
// before it has grown to full size.
using AliasedFileArray = std::list<AliasedFile>;


// Checks for alias block files, modifies the project if the
// user requests it, and returns True if the user continues.
// Returns false if the user clicks Cancel, meaning that they do
// not want to go ahead with the Save/Save As operation.
bool ShowDependencyDialogIfNeeded(SneedacityProject *project,
                                  bool isSaving);

// Returns a list of aliased files associated with a project.
void FindDependencies(SneedacityProject *project,
                      AliasedFileArray &outAliasedFiles);

#endif
