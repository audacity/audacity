/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORT__
#define __AUDACITY_EXPORT__

#include <functional>
#include <vector>
#include <wx/filename.h> // member variable
#include "Identifier.h"
#include "FileNames.h" // for FileTypes

#include "Registry.h"
#include "ExportPlugin.h"

class AudacityProject;
class WaveTrack;
class ExportProcessorDelegate;
namespace MixerOptions {
class Downmix;
}
using MixerSpec = MixerOptions::Downmix;
using WaveTrackConstArray = std::vector < std::shared_ptr < const WaveTrack > >;

using ExportPluginArray = std::vector < std::unique_ptr< ExportPlugin > >;

class IMPORT_EXPORT_API ExportTaskBuilder final
{
public:

    ExportTaskBuilder();
    ~ExportTaskBuilder();

    ExportTaskBuilder& SetFileName(const wxFileName& filename);
    ExportTaskBuilder& SetRange(double t0, double t1, bool selectedOnly = false) noexcept;
    ExportTaskBuilder& SetParameters(ExportProcessor::Parameters parameters) noexcept;
    ExportTaskBuilder& SetNumChannels(unsigned numChannels) noexcept;
    ExportTaskBuilder& SetPlugin(const ExportPlugin* plugin, int format = 0) noexcept;
    ExportTaskBuilder& SetTags(const Tags* tags) noexcept;
    ExportTaskBuilder& SetSampleRate(double sampleRate) noexcept;
    ExportTaskBuilder& SetMixerSpec(MixerOptions::Downmix* mixerSpec) noexcept;

    ExportTask Build(AudacityProject& project);

private:
    wxFileName mFileName;
    double mT0 {};
    double mT1 {};
    bool mSelectedOnly{};
    unsigned mNumChannels{ 1 };
    double mSampleRate{ 44100 };
    ExportProcessor::Parameters mParameters;
    const ExportPlugin* mPlugin{};
    int mFormat{};
    MixerOptions::Downmix* mMixerSpec{};//Should be const
    const Tags* mTags{};
};

void IMPORT_EXPORT_API ShowExportErrorDialog(const TranslatableString& message, const TranslatableString& caption, bool allowReporting);

void IMPORT_EXPORT_API ShowExportErrorDialog(const TranslatableString& message, const TranslatableString& caption,
                                             const ManualPageID& helpPageId, bool allowReporting);

void IMPORT_EXPORT_API ShowDiskFullExportErrorDialog(const wxFileNameWrapper& fileName);

#endif
