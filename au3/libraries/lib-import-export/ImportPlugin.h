/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportPlugin.h

  Joshua Haberman
  Leland Lucius

*******************************************************************//**

\file ImportPlugin.h
\brief
  The interface that all file import "plugins" (if you want to call
  them that) must implement.  Defines ImportFileHandle, ImportPlugin,
  UnusableImportPlugin, ImportPluginList and UnusableImportPluginList.

  Since this is part of libaudacity, it must not use any GUI parts
  of wxWidgets.

*//****************************************************************//**

\class ImportFileHandle
\brief Base class for FlacImportFileHandle, LOFImportFileHandle,
MP3ImportFileHandle, OggImportFileHandle and PCMImportFileHandle.
Gives API for sound file import.

*//****************************************************************//**

\class ImportPlugin
\brief Base class for FlacImportPlugin, LOFImportPlugin,
MP3ImportPlugin, OggImportPlugin and PCMImportPlugin.
Gives API for sound file import.

*//****************************************************************//**

\class UnusableImportPlugin
\brief Used in place of a real plug in for plug ins that have not
been compiled or are not available in this version of Audacity.  Has
enough information to identify the file extensions that would be used,
but little else.

*//*******************************************************************/

#ifndef __AUDACITY_IMPORTER__
#define __AUDACITY_IMPORTER__

#include "libraries/lib-file-formats/AcidizerTags.h"
#include "Identifier.h"
#include "Internat.h"
#include "wxArrayStringEx.h"
#include <memory>
#include <optional>

class AudacityProject;
class WaveTrackFactory;
class Track;
class TrackList;
class TranslatableString;
class Tags;

class ImportFileHandle;

class ImportProgressListener;

class IMPORT_EXPORT_API ImportPlugin /* not final */
{
public:

    // Get unique string ID of this plugin, usually it corresponds
    // to the underlying library, i.e. "libsndfile", "libflac", "libav"
    // These MUST NOT change across Audacity versions (but NEW IDs can
    // be added).
    virtual wxString GetPluginStringID() = 0;

    // Get a description of the file type this importer can import.
    // Examples: "Ogg Vorbis", "MP3", "Uncompressed PCM"
    virtual TranslatableString GetPluginFormatDescription() = 0;

    // Get a list of extensions this plugin expects to be able to
    // import.  If a filename matches any of these extensions,
    // this importer will get first dibs on importing it.
    virtual FileExtensions GetSupportedExtensions();

    //! User visible message suggesting what to do when a file type isn't recognized; default empty string
    /*! Should end with one newline if not empty */
    virtual TranslatableString FailureHint() const;

    bool SupportsExtension(const FileExtension& extension);

    // Open the given file, returning true if it is in a recognized
    // format, false otherwise.  This puts the importer into the open
    // state.
    virtual std::unique_ptr<ImportFileHandle> Open(
        const FilePath& Filename, AudacityProject*) = 0;

    virtual ~ImportPlugin();

protected:

    ImportPlugin(FileExtensions supportedExtensions);

    const FileExtensions mExtensions;
};

class WaveTrack;
using TrackHolders = std::vector<std::shared_ptr<Track> >;

class IMPORT_EXPORT_API ImportFileHandle /* not final */
{
public:

    using ByteCount = unsigned long long;

    virtual ~ImportFileHandle();

    virtual FilePath GetFilename() const = 0;

    virtual TranslatableString GetErrorMessage() const;

    // This is similar to GetPluginFormatDescription, but if possible the
    // importer will return a more specific description of the
    // specific file that is open.
    virtual TranslatableString GetFileDescription() = 0;

    // Return an estimate of how many bytes the file will occupy once
    // imported.  In principle this may exceed main memory, so don't use
    // size_t.
    virtual ByteCount GetFileUncompressedBytes() = 0;

    // Return number of elements in stream list
    virtual wxInt32 GetStreamCount() = 0;

    // Return stream descriptions list, before Import() is called
    virtual const TranslatableStrings& GetStreamInfo() = 0;

    // Set stream "import/don't import" flag, before Import() is called
    virtual void SetStreamUsage(wxInt32 StreamID, bool Use) = 0;

    // do the actual import, creating whatever tracks are necessary with
    // the WaveTrackFactory and calling the progress callback every iteration
    // through the importing loop
    // The given Tags and AcidizerTags structures may also be modified.
    // In case of errors or exceptions, it is not necessary to leave outTracks,
    // tags or acidTags unmodified. If resulting outTracks is not empty, then
    // each member of it must be a nonempty vector.
    virtual void Import(
        ImportProgressListener& progressListener, WaveTrackFactory* trackFactory, TrackHolders& outTracks, Tags* tags,
        std::optional<LibFileFormats::AcidizerTags>& acidTags) = 0;

    virtual void Cancel() = 0;

    virtual void Stop() = 0;
};

class IMPORT_EXPORT_API ImportFileHandleEx : public ImportFileHandle
{
    FilePath mFilename;
    bool mCancelled{ false };
    bool mStopped{ false };
public:
    ImportFileHandleEx(const FilePath& filename);

    FilePath GetFilename() const override;
    void Cancel() override;
    void Stop() override;

protected:
    void BeginImport();
    bool IsCancelled() const noexcept;
    bool IsStopped() const noexcept;
};

class IMPORT_EXPORT_API UnusableImportPlugin
{
public:
    UnusableImportPlugin(
        const TranslatableString& formatName, FileExtensions extensions)
        : mFormatName(formatName),
        mExtensions(std::move(extensions))
    {
    }

    TranslatableString GetPluginFormatDescription()
    {
        return mFormatName;
    }

    bool SupportsExtension(const FileExtension& extension)
    {
        return mExtensions.Index(extension, false) != wxNOT_FOUND;
    }

private:
    TranslatableString mFormatName;
    const FileExtensions mExtensions;
};

#endif
