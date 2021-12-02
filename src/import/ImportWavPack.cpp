/**********************************************************************

  Audacity: A Digital Audio Editor
  Audacity(R) is copyright (c) 1999-2021 Audacity Team.
  License: GPL-v2-or-later.  See License.txt.

  ImportWavPack.cpp

  Subhradeep Chakraborty

*//****************************************************************//**

\class WavPackImportFileHandle
\brief An ImportFileHandle for WavPack data

*//****************************************************************//**

\class WavPackImportPlugin
\brief An ImportPlugin for WavPack data

*//*******************************************************************/

#include "Import.h"
#include "ImportPlugin.h"

#include<wx/string.h>

#include "Prefs.h"
#include "../Tags.h"
#include "../WaveTrack.h"

#define DESC XO("WavPack files")

static const auto exts = {
   wxT("wv")
};

#ifndef USE_WAVPACK

static Importer::RegisteredUnusableImportPlugin registered
{
   std::make_unique<UnusableImportPlugin>(DESC, FileExtensions(exts.begin(), exts.end()))
};

#else

extern "C" {
#include<wavpack.h>
}


class WavPackImportPlugin final : public ImportPlugin
{
public:
   WavPackImportPlugin();
   ~WavPackImportPlugin();

   wxString GetPluginStringID() override;
   TranslatableString GetPluginFormatDescription() override; 
   std::unique_ptr<ImportFileHandle> Open(
     const FilePath &Filename, AudacityProject*) override;
};

using NewChannelGroup = std::vector< std::shared_ptr<WaveTrack> >;

class WavPackImportFileHandle final : public ImportFileHandle
{
public:
   WavPackImportFileHandle(const FilePath &filename);
   ~WavPackImportFileHandle();

   TranslatableString GetFileDescription() override;
   ByteCount GetFileUncompressedBytes() override;
   ProgressResult Import(WaveTrackFactory *trackFactory, TrackHolders &outTracks, Tags *tags) override;
   wxInt32 GetStreamCount() override;
   const TranslatableStrings &GetStreamInfo() override;
   void SetStreamUsage(wxInt32 StreamID, bool Use) override;
};

#endif