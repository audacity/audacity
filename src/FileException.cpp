//
//  FileException.cpp
//  
//
//  Created by Paul Licameli on 11/22/16.
//
//

#include "Audacity.h"
#include "FileException.h"

#include "Internat.h"
#include "Prefs.h"

FileException::~FileException()
{
}

wxString FileException::ErrorMessage() const
{
   wxString format;
   switch (cause) {
      case Cause::Open:
         format = _("Audacity failed to open a file in %s.");
         break;
      case Cause::Read:
         format = _("Audacity failed to read from a file in %s.");
         break;
      case Cause::Write:
         format =
_("Audacity failed to write to a file.\n"
  "Perhaps %s is not writable or the disk is full.");
         break;
      case Cause::Rename:
         format =
_("Audacity successfully wrote a file in %s but failed to rename it as %s.");
      default:
         break;
   }
   wxString target;

#ifdef __WXMSW__

   // Drive letter plus colon
   target = fileName.GetVolume() + wxT(":");

#else

   // Shorten the path, arbitrarily to 3 components
   auto path = fileName;
   path.SetFullName(wxString{});
   while(path.GetDirCount() > 3)
      path.RemoveLastDir();
   target = path.GetFullPath();

#endif

   return wxString::Format(
      format, target, renameTarget.GetFullName() );
}

