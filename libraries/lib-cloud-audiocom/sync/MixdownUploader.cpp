/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MixdownUploader.cpp

  Dmitry Vedenko

**********************************************************************/

#include "MixdownUploader.h"

#include <rapidjson/document.h>

#include "WaveTrack.h"

#include "CodeConversions.h"

#include "ServiceConfig.h"

#include "Export.h"
#include "ExportPluginRegistry.h"
#include "ExportUtils.h"
#include "ExportProgressUI.h"

#include "ProjectRate.h"

#include "UploadService.h"
#include "DataUploader.h"

#include "sync/CloudSyncUI.h"

namespace cloud::audiocom::sync
{
namespace
{
std::string GenerateTempPath(FileExtension extension)
{
   const auto tempPath = GetUploadTempPath();

   wxFileName fileName(
      tempPath,
      wxString::Format(
         "%lld", std::chrono::system_clock::now().time_since_epoch().count()),
      extension);

   fileName.Mkdir(0700, wxPATH_MKDIR_FULL);

   if (fileName.Exists())
   {
      if (!wxRemoveFile(fileName.GetFullPath()))
         return {};
   }

   return audacity::ToUTF8(fileName.GetFullPath());
}

int CalculateChannels(const TrackList& trackList)
{
   auto range = trackList.Any<const WaveTrack>();
   return std::all_of(
             range.begin(), range.end(),
             [](const WaveTrack* track)
             { return IsMono(*track) && track->GetPan() == 0; }) ?
             1 :
             2;
}
} // namespace


class MixdownUploader::DataExporter final :
    public ExportProcessorDelegate
{
public:
   DataExporter(MixdownUploader& parent, ExportTask task)
       : mParent { parent }
       , mTask { std::move(task) }
       , mExportThread { [this] { ExportTread(); } }
   {
   }

   ~DataExporter() override
   {
      mExportThread.join();
   }

   void Cancel()
   {
      mCancelled.store(true, std::memory_order_release);
   }

   ExportResult GetResult() const
   {
      return mResult;
   }

   void OnComplete(ExportResult result)
   {
      mResult = result;

      if (result == ExportResult::Success)
      {
         mParent.UploadMixdown();
      }
      else
      {
         mParent.mOnComplete({}, false);
      }
   }

   void SetStatusString(const TranslatableString& str) override
   {
   }

   bool IsCancelled() const override
   {
      return mCancelled.load(std::memory_order_acquire);
   }

   bool IsStopped() const override
   {
      return false;
   }

   void OnProgress(double value) override
   {
      mParent.SetProgress(value);
   }

   void ExportTread()
   {
      try {
         auto future = mTask.get_future();
         mTask(*this);
         const auto result = future.get();

         BasicUI::CallAfter([this, result] { OnComplete(result); });
      }
      catch (const ExportDiskFullError& error)
      {
         HandleExportDiskFullError(error);
      }
      catch (const ExportErrorException& error)
      {
         HandleExportError(error);
      }
      catch (const ExportException& error)
      {
         HandleExportException(error);
      }
      catch (...)
      {
         HandleUnkonwnException();
      }
   }

   void HandleExportDiskFullError(const ExportDiskFullError& error)
   {
      BasicUI::CallAfter(
         [this, fileName = error.GetFileName()]
         {
            ShowDiskFullExportErrorDialog(fileName);
            mParent.mOnComplete({}, false);
         });
   }

   void HandleExportError(const ExportErrorException& error)
   {
      BasicUI::CallAfter(
         [this, message = error.GetMessage(), helpPage = error.GetHelpPageId()]
         {
            ShowExportErrorDialog(message, XO("Export failed"), helpPage, true);
            mParent.mOnComplete({}, false);
         });
   }

   void HandleExportException(const ExportException& error)
   {
      BasicUI::CallAfter(
         [this, message = error.What()]
         {
            ShowExportErrorDialog(Verbatim(message), XO("Export failed"), true);
            mParent.mOnComplete({}, false);
         });
   }

   void HandleUnkonwnException()
   {
      BasicUI::CallAfter([] { BasicUI::ShowMessageBox(XO("Export error")); });
   }

private:
   MixdownUploader& mParent;
   ExportTask mTask;
   std::thread mExportThread;

   std::atomic<bool> mCancelled { false };
   ExportResult mResult { ExportResult::Stopped };
};

MixdownUploader::MixdownUploader(
   Tag, CloudSyncUI& ui, const ServiceConfig& config,
   const AudacityProject& project, const UploadUrls& urls,
   MixdownUploaderCompleteCallback onComplete)
    : mCloudSyncUI { ui }
    , mServiceConfig { config }
    , mProject { project }
    , mUploadUrls { urls }
    , mOnComplete { std::move(onComplete) }
{
   ExportProject();
}

MixdownUploader::~MixdownUploader()
{
   if (wxFileExists(mExportedFilePath))
      wxRemoveFile(mExportedFilePath);
}

std::unique_ptr<MixdownUploader> MixdownUploader::Upload(
   CloudSyncUI& ui, const ServiceConfig& config,
   const AudacityProject& project, const UploadUrls& urls,
   MixdownUploaderCompleteCallback onComplete)
{
   if (!onComplete)
      onComplete = [](auto...) {};

   return std::make_unique<MixdownUploader>(
      Tag {}, ui, config, project, urls, std::move(onComplete));
}

void MixdownUploader::SetProgress(double progress)
{
   mCurrentProgress.store(progress, std::memory_order_release);

   if (!mProgressUpdateQueued.exchange(true, std::memory_order_acq_rel))
      BasicUI::CallAfter(
         [this]
         {
            if (!mCloudSyncUI.OnMixdownProgress(
                   mCurrentProgress.load(std::memory_order_acquire)))
            {
               if (mExporting.load(std::memory_order_acquire) && mDataExporter)
                  mDataExporter->Cancel();
               else
                  mUploadCancelled.store(true, std::memory_order_release);
            }

            mProgressUpdateQueued.store(false, std::memory_order_release);
         });
}

void MixdownUploader::ExportProject()
{
   mCloudSyncUI.OnMixdownStarted();
   mCloudSyncUI.SetMixdownProgressMessage(XO("Exporting project..."));

   auto& tracks = TrackList::Get(mProject);

   const double t0 = 0.0;
   const double t1 = tracks.GetEndTime();

   const int nChannels = CalculateChannels(tracks);

   auto hasMimeType = [](const auto&& mimeTypes, const std::string& mimeType)
   {
      return std::find(mimeTypes.begin(), mimeTypes.end(), mimeType) !=
             mimeTypes.end();
   };

   const auto& registry = ExportPluginRegistry::Get();

   for (const auto& preferredMimeType :
        GetServiceConfig().GetPreferredAudioFormats(false))
   {
      auto config = GetServiceConfig().GetExportConfig(preferredMimeType);
      ExportProcessor::Parameters parameters;
      auto pluginIt = std::find_if(
         registry.begin(), registry.end(),
         [&](auto t)
         {
            auto [plugin, formatIndex] = t;
            parameters.clear();
            return hasMimeType(
                      plugin->GetMimeTypes(formatIndex), preferredMimeType) &&
                   plugin->ParseConfig(formatIndex, config, parameters);
         });

      if (pluginIt == registry.end())
         continue;

      const auto [plugin, formatIndex] = *pluginIt;

      const auto formatInfo = plugin->GetFormatInfo(formatIndex);
      const auto path = GenerateTempPath(formatInfo.extensions[0]);

      if (path.empty())
         continue;

      auto builder = ExportTaskBuilder {}
                        .SetParameters(parameters)
                        .SetNumChannels(nChannels)
                        .SetSampleRate(ProjectRate::Get(mProject).GetRate())
                        .SetPlugin(plugin)
                        .SetFileName(audacity::ToWXString(path))
                        .SetRange(t0, t1, false);

      mExportedFilePath = path;

      mDataExporter = std::make_unique<DataExporter>(
         *this, builder.Build(const_cast<AudacityProject&>(mProject)));

      return;
   }

   if (!mDataExporter)
      mOnComplete({}, false);
}
void MixdownUploader::UploadMixdown()
{
   mCloudSyncUI.SetMixdownProgressMessage(XO("Uploading mixdown..."));
   DataUploader::Get().Upload(
      mServiceConfig, mUploadUrls, mExportedFilePath,
      [this](UploadResult result)
      {
         BasicUI::CallAfter([this, result = std::move(result)]
            {
               mCloudSyncUI.OnMixdownFinished();
               mOnComplete(result.ErrorMessage, result.Code == UploadResultCode::Success);
            });
      },
      {});
}
} // namespace cloud::audiocom::sync
