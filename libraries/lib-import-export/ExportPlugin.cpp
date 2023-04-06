/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPlugin.cpp

  Dominic Mazzoni

**********************************************************************/

#include "ExportPlugin.h"
#include "ExportProgressListener.h"

ExportPlugin::ExportPlugin() = default;
ExportPlugin::~ExportPlugin() = default;

bool ExportPlugin::CheckFileName(wxFileName&, int)
{
  return true;
}

TranslatableString ExportPluginEx::GetStatusString() const
{
   return mStatus;
}

TranslatableString ExportPluginEx::GetErrorString() const
{
   return mError;
}

void ExportPluginEx::Cancel()
{
   if(!mStopped)
      mCancelled = true;
}

void ExportPluginEx::Stop()
{
   if(!mCancelled)
      mStopped = true;
}

void ExportPluginEx::ExportBegin()
{
   mCancelled = false;
   mStopped = false;
   mStatus = { };
}

void ExportPluginEx::ExportFinish(ExportProgressListener& progressListener)
{
   if(mCancelled)
      progressListener.OnExportResult(ExportProgressListener::ExportResult::Cancelled);
   else if(mStopped)
      progressListener.OnExportResult(ExportProgressListener::ExportResult::Stopped);
   else
      progressListener.OnExportResult(ExportProgressListener::ExportResult::Success);
}

bool ExportPluginEx::IsCancelled() const noexcept
{
   return mCancelled;
}

bool ExportPluginEx::IsStopped() const noexcept
{
   return mStopped;
}

void ExportPluginEx::SetStatusString(const TranslatableString &status)
{
   mStatus = status;
}

void ExportPluginEx::SetErrorString(const TranslatableString &error)
{
   mError = error;
}
