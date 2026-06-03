/*!
  @file FileException.cpp
  @brief implements FileException


  Created by Paul Licameli on 11/22/16.

*/

#include "FileException.h"
#include "FileNames.h"

#include "au3-preferences/Prefs.h"

FileException::~FileException()
{
}

::TranslatableString FileException::ErrorMessage() const
{
    TranslatableString format;
    switch (cause) {
    case Cause::Open:
        format = TranslatableString("files", "Audacity failed to open a file in %1.");
        break;
    case Cause::Read:
        format = TranslatableString("files", "Audacity failed to read from a file in %1.");
        break;
    case Cause::Write:
        return WriteFailureMessage(fileName);
    case Cause::Rename:
        format
            =::TranslatableString("files", "Audacity successfully wrote a file in %1 but failed to rename it as %2.");
    default:
        break;
    }

    return format.Format(
        FileNames::AbbreviatePath(fileName), renameTarget.GetFullName());
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

::TranslatableString
FileException::WriteFailureMessage(const wxFileName& fileName)
{
    return ::TranslatableString("files", "Audacity failed to write to a file.\nPerhaps %1 is not writable or the disk is full.").arg(FileNames::AbbreviatePath(fileName));
}
