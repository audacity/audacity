//
//  FileException.cpp
//  
//
//  Created by Paul Licameli on 11/22/16.
//
//

#include "Audacity.h"
#include "FileException.h"

FileException::~FileException()
{
}

std::unique_ptr< AudacityException > FileException::Move()
{
   return std::unique_ptr< AudacityException >
      { safenew FileException{ std::move( *this ) } };
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
         format = _("Audacity failed to write to a file in %s.");
         break;
      case Cause::Rename:
         format =
_("Audacity successfully wrote a file in %s but failed to rename it as %s.");
      default:
         break;
   }
   wxString target = fileName.GetVolume();
   if (target.empty()) {
      // Shorten the path, arbitrarily to 3 components
      auto path = fileName;
      path.SetFullName(wxString{});
      while(path.GetDirCount() > 3)
         path.RemoveLastDir();
      target = path.GetFullPath();
   }
   return wxString::Format(
      format, target, renameTarget.GetFullName() );
}

