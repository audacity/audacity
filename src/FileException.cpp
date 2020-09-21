/*!
  @file FileException.cpp
  @brief implements FileException
  

  Created by Paul Licameli on 11/22/16.

*/

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
           "Perhaps %s is not writable or the disk is full.\n"
           "For tips on freeing up space, click the help button."
         );
         break;
      case Cause::Rename:
         format =
XO("Audacity successfully wrote a file in %s but failed to rename it as %s.");
      default:
         break;
   }

   return format.Format(
      AbbreviatePath(fileName), renameTarget.GetFullName() );
}

wxString FileException::ErrorHelpUrl() const
{
   switch (cause) {
   case Cause::Open:
   case Cause::Read:
      return "Error:_Opening_or_reading_file";
      break;
   case Cause::Write:
   case Cause::Rename:
      return "Error:_Disk_full_or_not_writable";
   default:
      break;
   }

   return "";
}


wxString FileException::AbbreviatePath( const wxFileName &fileName )
{
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
   return target;
}
