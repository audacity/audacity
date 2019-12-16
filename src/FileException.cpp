//
//  FileException.cpp
//  
//
//  Created by Paul Licameli on 11/22/16.
//
//

#include "Audacity.h"
#include "FileException.h"

#include "Prefs.h"

FileException::~FileException()
{
}

TranslatableString FileException::ErrorMessage() const
{
   TranslatableString format;
   switch (cause) {
      case Cause::Open:
         format = XO("Audacity failed to open a file in %s.");
         break;
      case Cause::Read:
         format = XO("Audacity failed to read from a file in %s.");
         break;
      case Cause::Write:
         format =
XO("Audacity failed to write to a file.\n"
  "Perhaps %s is not writable or the disk is full.");
         break;
      case Cause::Rename:
         format =
XO("Audacity successfully wrote a file in %s but failed to rename it as %s.");
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

   return format.Format( target, renameTarget.GetFullName() );
}

