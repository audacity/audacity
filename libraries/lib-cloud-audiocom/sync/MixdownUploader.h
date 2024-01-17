/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MixdownUploader.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <atomic>
#include <functional>
#include <memory>
#include <string>

#include "CloudSyncUtils.h"

class AudacityProject;

namespace cloud::audiocom
{
class ServiceConfig;
}

namespace cloud::audiocom::sync
{
class CloudSyncUI;

using MixdownUploaderCompleteCallback = std::function<void(std::string, bool)>;

class MixdownUploader final
{
   struct Tag {};

public:
   MixdownUploader(
      Tag, CloudSyncUI& ui, const ServiceConfig& config,
      const AudacityProject& project, const UploadUrls& urls,
      MixdownUploaderCompleteCallback onComplete);

   ~MixdownUploader();

   static std::unique_ptr<MixdownUploader> Upload(
      CloudSyncUI& ui, const ServiceConfig& config,
      const AudacityProject& project, const UploadUrls& urls,
      MixdownUploaderCompleteCallback onComplete);

private:
   void SetProgress(double progress);
   void ExportProject();
   void UploadMixdown();

   CloudSyncUI& mCloudSyncUI;
   const ServiceConfig& mServiceConfig;
   const AudacityProject& mProject;

   UploadUrls mUploadUrls;

   MixdownUploaderCompleteCallback mOnComplete;

   class DataExporter;
   std::unique_ptr<DataExporter> mDataExporter;

   std::string mExportedFilePath;

   std::atomic<double> mCurrentProgress;
   std::atomic<bool> mProgressUpdateQueued { false };
   std::atomic<bool> mExporting { true };
   std::atomic<bool> mUploadCancelled { false };
}; // class MixdownUploader
} // namespace cloud::audiocom::sync
