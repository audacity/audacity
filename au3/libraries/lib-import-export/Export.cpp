/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.cpp

  Dominic Mazzoni

*******************************************************************//**

\class Export
\brief Main class to control the export function.

*//*****************************************************************/

#include "Export.h"

#include <numeric>

#include "BasicUI.h"
#include "ExportPluginRegistry.h"
#include "Mix.h"
#include "Project.h"
#include "WaveTrack.h"
#include "wxFileNameWrapper.h"
#include "StretchingSequence.h"

#include "ExportUtils.h"

ExportTaskBuilder::ExportTaskBuilder() = default;
ExportTaskBuilder::~ExportTaskBuilder() = default;

ExportTaskBuilder& ExportTaskBuilder::SetFileName(const wxFileName& filename)
{
    mFileName = filename;
    return *this;
}

ExportTaskBuilder& ExportTaskBuilder::SetRange(double t0, double t1, bool selectedOnly) noexcept
{
    mT0 = t0;
    mT1 = t1;
    mSelectedOnly = selectedOnly;
    return *this;
}

ExportTaskBuilder& ExportTaskBuilder::SetParameters(ExportProcessor::Parameters parameters) noexcept
{
    mParameters = std::move(parameters);
    return *this;
}

ExportTaskBuilder& ExportTaskBuilder::SetNumChannels(unsigned int numChannels) noexcept
{
    mNumChannels = numChannels;
    return *this;
}

ExportTaskBuilder& ExportTaskBuilder::SetPlugin(const ExportPlugin* plugin, int format) noexcept
{
    mPlugin = plugin;
    mFormat = format;
    return *this;
}

ExportTaskBuilder& ExportTaskBuilder::SetMixerSpec(MixerOptions::Downmix* mixerSpec) noexcept
{
    mMixerSpec = mixerSpec;
    return *this;
}

ExportTaskBuilder& ExportTaskBuilder::SetSampleRate(double sampleRate) noexcept
{
    mSampleRate = sampleRate;
    return *this;
}

ExportTaskBuilder& ExportTaskBuilder::SetTags(const Tags* tags) noexcept
{
    mTags = tags;
    return *this;
}

ExportTask ExportTaskBuilder::Build(AudacityProject& project)
{
    //File rename stuff should be moved out to somewhere else...
    auto filename = mFileName;

    //For safety, if the file already exists we use temporary filename
    //and replace original one export succeeded
    int suffix = 0;
    while (filename.FileExists()) {
        filename.SetName(mFileName.GetName()
                         + wxString::Format(wxT("%d"), suffix));
        suffix++;
    }

    auto processor = mPlugin->CreateProcessor(mFormat);
    if (!processor->Initialize(project,
                               mParameters,
                               mFileName.GetFullPath(),
                               mT0, mT1, mSelectedOnly,
                               mSampleRate, mMixerSpec ? mMixerSpec->GetNumChannels() : mNumChannels,
                               mMixerSpec,
                               mTags)) {
        return ExportTask([](ExportProcessorDelegate&){ return ExportResult::Cancelled; });
    }

    return ExportTask([actualFilename = filename,
                       targetFilename = mFileName,
                       processor = std::shared_ptr<ExportProcessor>(processor.release())]
                      (ExportProcessorDelegate& delegate)
    {
        auto result = ExportResult::Error;
        auto cleanup = finally([&] {
            if (result == ExportResult::Success || result == ExportResult::Stopped) {
                if (actualFilename != targetFilename) {
                    //may fail...
                    ::wxRenameFile(actualFilename.GetFullPath(),
                                   targetFilename.GetFullPath(),
                                   true);
                }
            } else {
                ::wxRemoveFile(actualFilename.GetFullPath());
            }
        });
        result = processor->Process(delegate);
        return result;
    });
}

void ShowDiskFullExportErrorDialog(const wxFileNameWrapper& fileName)
{
    BasicUI::ShowErrorDialog({},
                             XO("Warning"),
                             FileException::WriteFailureMessage(fileName),
                             "Error:_Disk_full_or_not_writable"
                             );
}

void ShowExportErrorDialog(const TranslatableString& message,
                           const TranslatableString& caption,
                           bool allowReporting)
{
    ShowExportErrorDialog(message, caption, {}, allowReporting);
}

void ShowExportErrorDialog(const TranslatableString& message,
                           const TranslatableString& caption,
                           const ManualPageID& helpPageId,
                           bool allowReporting)
{
    using namespace BasicUI;
    ShowErrorDialog({},
                    caption,
                    message,
                    helpPageId,
                    ErrorDialogOptions { allowReporting ? ErrorDialogType::ModalErrorReport : ErrorDialogType::ModalError });
}
