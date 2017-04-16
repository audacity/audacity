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
         format = _("Audacity failed to open a file at %s.\n");
         break;
      case Cause::Read:
         format = _("Audacity failed to read from a file at %s.\n");
         break;
      case Cause::Write:
         format = _(
"Audacity failed to write to a file at %s.\nAttempt this operation again after removing unnecessary files.\nOne way to do that is with the Discard buttons in the History dialog.\nSee the View menu.");
         break;
      case Cause::Rename:
         format = _(
"Audacity successfully wrote the file %s but failed to rename it as %s.");
      default:
         break;
   }
   return wxString::Format(
      format, fileName.GetFullPath(), renameTarget.GetFullPath() );
}

