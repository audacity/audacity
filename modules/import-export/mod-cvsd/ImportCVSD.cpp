/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ImportCVSD.cpp

*/

#include "Import.h"

#include <wx/log.h>
#include <wx/setup.h> // see next comment
/* ffile.h must be included AFTER at least one other wx header that includes
 * wx/setup.h, otherwise #ifdefs erroneously collapse it to nothing. This is
 * a bug in wxWidgets (ffile.h should itself include wx/setup.h), and it
 * was a bitch to track down. */
#include <wx/ffile.h>

#define DESC XO("CVSD files")

static const auto exts = {
  wxT("cvsd"), wxT("cvsdm")
};

#include "ImportPlugin.h"
#include "ImportProgressListener.h"
#include "ImportUtils.h"

class CVSDImportPlugin final: public ImportPlugin
{
public:
  CVSDImportPlugin()
    : ImportPlugin( FileExtensions( exts.begin(), exts.end() ) )
  {
  }

  ~ CVSDImportPlugin();

  wxString GetPluginStringID() override { return wxT("cvsd"); }
  TranslatableString GetPluginFormatDescription() override;
  std::unique_ptr<ImportFileHandle> Open(
     const FilePath &Filename, AudacityProject*) override;
};

class CVSDImportFileHandle final : public ImportFileHandleEx
{
public:
  CVSDImportFileHandle(const FilePath & filename)
  :  ImportFileHandleEx(filename)
  {
  }

  ~CVSDImportFileHandle() override;

  TranslatableString GetFileDescription() override;
  ByteCount GetFileUncompressedBytes() override;
  void Import(
     ImportProgressListener& progressListener, WaveTrackFactory* trackFactory,
     TrackHolders& outTracks, Tags* tags,
     std::optional<LibFileFormats::AcidizerTags>& outAcidTags) override;

  wxInt32 GetStreamCount() override;

  const TranslatableStrings &GetStreamInfo() override
  {
    return mStreamInfo;
  }

  void SetStreamUsage(wxInt32 StreamID, bool Use) override;
private:
  std::unique_ptr<wxFFile> mFile;

  ArrayOf<int> mStreamUsage;
  TranslatableStrings mStreamInfo;
  std::vector<TrackListHolder> mStreams;
};

class CVSDImportFileHandle final : public ImportFileHandleEx
{
public:
  CVSDImportFileHandle(const FilePath & filename)
  :  ImportFileHandleEx(filename)
  {
  }
  ~CVSDImportFileHandle() override;

  TranslatableString GetFileDescription() override;
  ByteCount GetFileUncompressedBytes() override;

  void Import(
     ImportProgressListener& progressListener, WaveTrackFactory* trackFactory,
     TrackHolders& outTracks, Tags* tags,
     std::optional<LibFileFormats::AcidizerTags>& outAcidTags) override;

  wxInt32 GetStreamCount() override;
  void SetStreamUsage(wxInt32 StreamID, bool Use) override;

private:
  ArrayOf<int> mStreamUsage;
  TranslatableStrings mStreamInfo;
  std::vector<TrackListHolder> mStreams;
};