/**********************************************************************

 Audacity: A Digital Audio Editor

 TempDirectory.cpp

 Paul Licameli split from FileNames.cpp

 **********************************************************************/

#include "TempDirectory.h"

#include "FileNames.h"
#include "BasicUI.h"

namespace {
struct TempDirChangedPublisher final : Observer::Publisher<FilePath>
{
    void UpdateTempPath(const FilePath& path)
    {
        if (prevPath != path) {
            Publish(path);
            prevPath = path;
        }
    }

    FilePath prevPath;
};

TempDirChangedPublisher& GetTempDirChangedPublisher()
{
    static TempDirChangedPublisher publisher;
    return publisher;
}
}

static wxString& TempDirPath()
{
    static wxString path;
    return path;
}

/// Returns the directory used for temp files.
/// \todo put a counter in here to see if it gets used a lot.
/// if it does, then maybe we should cache the path name
/// each time.
wxString TempDirectory::TempDir()
{
    auto& path = TempDirPath();
    if (gPrefs && path.empty()) {
        path
            =gPrefs->Read(PreferenceKey(FileNames::Operation::Temp,
                                        FileNames::PathType::_None), wxT(""));
    }

    if (FileNames::IsOnFATFileSystem(path)) {
        BasicUI::ShowErrorDialog({},
                                 XO("Unsuitable"),
                                 XO("The temporary files directory is on a FAT formatted drive.\n"
                                    "Resetting to default location."),
                                 "Error:_Unsuitable_drive"
                                 );

        path = DefaultTempDir();
        UpdateDefaultPath(FileNames::Operation::Temp, path);
    }

    return FileNames::MkDir(path);
}

void TempDirectory::ResetTempDir()
{
    TempDirPath().clear();
}

/** \brief Default temp directory */
static FilePath sDefaultTempDir;

const FilePath& TempDirectory::DefaultTempDir()
{
    return sDefaultTempDir;
}

void TempDirectory::SetDefaultTempDir(const FilePath& tempDir)
{
    sDefaultTempDir = tempDir;
    GetTempDirChangedPublisher().UpdateTempPath(tempDir);
}

// We now disallow temp directory name that puts it where cleaner apps will
// try to clean out the files.
bool TempDirectory::IsTempDirectoryNameOK(const FilePath& Name)
{
    if (Name.empty()) {
        return false;
    }

    wxFileName tmpFile;
    tmpFile.AssignTempFileName(wxT("nn"));
    // use Long Path to expand out any abbreviated long substrings.
    wxString BadPath = tmpFile.GetLongPath();
    ::wxRemoveFile(tmpFile.GetFullPath());

#ifdef __WXMAC__
    // This test is to fix bug 1220 on a 1.x to 2.x to 2.1.3 upgrade.
    // It is less permissive than we could be as it stops a path
    // with this string ANYWHERE within it rather than excluding just
    // the paths that the earlier Audacities used to create.
    if (Name.Contains("/tmp/")) {
        return false;
    }
    BadPath = BadPath.BeforeLast('/') + "/";
    wxFileName cmpFile(Name);
    wxString NameCanonical = cmpFile.GetLongPath() + "/";
#else
    BadPath = BadPath.BeforeLast('\\') + "\\";
    wxFileName cmpFile(Name);
    wxString NameCanonical = cmpFile.GetLongPath() + "\\";
#endif

    if (FATFilesystemDenied(NameCanonical,
                            XO("The temporary files directory is on a FAT formatted drive.\n"
                               "Resetting to default location."))) {
        return false;
    }

    return !(NameCanonical.StartsWith(BadPath));
}

wxString TempDirectory::UnsavedProjectFileName()
{
    wxFileName fn(TempDir(),
                  FileNames::CreateUniqueName(wxT("New Project"), FileNames::UnsavedProjectExtension()));

    return fn.GetFullPath();
}

bool TempDirectory::FATFilesystemDenied(const FilePath& path,
                                        const TranslatableString& msg, const BasicUI::WindowPlacement& placement)
{
    if (FileNames::IsOnFATFileSystem(path)) {
        BasicUI::ShowErrorDialog(placement,
                                 XO("Unsuitable"),
                                 XO("%s\n\nFor tips on suitable drives, click the help button.").Format(msg),
                                 "Error:_Unsuitable_drive"
                                 );

        return true;
    }

    return false;
}

Observer::Publisher<FilePath>& TempDirectory::GetTempPathObserver()
{
    return GetTempDirChangedPublisher();
}
