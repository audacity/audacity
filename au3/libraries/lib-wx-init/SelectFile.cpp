/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file SelectFile.cpp

 Paul Licameli split from FileNames.cpp

 **********************************************************************/

#include "SelectFile.h"
#include "FileNames.h"
#include "FileDialog/FileDialog.h"
#include "AudacityMessageBox.h"

FilePath
SelectFile(FileNames::Operation op,
           const TranslatableString& message,
           const FilePath& default_path,
           const FilePath& default_filename,
           const FileExtension& default_extension,
           const FileTypes& fileTypes,
           int flags,
           wxWindow* parent)
{
    return WithDefaultPath(op, default_path, [&](const FilePath& path) {
        wxString filter;
        if (!default_extension.empty()) {
            filter = wxT("*.") + default_extension;
        }
        return FileSelector(
            message.Translation(), path, default_filename, filter,
            FormatWildcard(fileTypes),
            flags, parent, wxDefaultCoord, wxDefaultCoord);
    });
}

#if defined(__WXMSW__)
static wxCharBuffer mFilename;

//
// On Windows, wxString::mb_str() can return a NULL pointer if the
// conversion to multi-byte fails.  So, based on direction intent,
// returns a pointer to an empty string or prompts for a NEW name.
//
char* VerifyFilename(const wxString& s, bool input)
{
    static wxCharBuffer buf;
    wxString name = s;

    if (input) {
        if ((char*)(const char*)name.mb_str() == NULL) {
            name = wxEmptyString;
        }
    } else {
        wxFileName ff(name);
        FileExtension ext;
        while ((char*)(const char*)name.mb_str() == NULL) {
            AudacityMessageBox(
                XO(
                    "The specified filename could not be converted due to Unicode character use."));

            ext = ff.GetExt();
            name = SelectFile(FileNames::Operation::_None,
                              XO("Specify New Filename:"),
                              wxEmptyString,
                              name,
                              ext,
                              { ext.empty()
                                ? FileNames::AllFiles
                                : FileNames::FileType { {}, { ext } }
                              },
                              wxFD_SAVE | wxRESIZE_BORDER,
                              wxGetTopLevelParent(NULL));
        }
    }

    mFilename = name.mb_str();

    return (char*)(const char*)mFilename;
}

#endif
