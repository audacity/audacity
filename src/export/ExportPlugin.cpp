/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPlugin.cpp

  Dominic Mazzoni

**********************************************************************/

#include "ExportPlugin.h"
#include "ShuttleGui.h"
#include "ExportProgressListener.h"

ExportPlugin::ExportPlugin() = default;
ExportPlugin::~ExportPlugin() = default;

bool ExportPlugin::CheckFileName(wxFileName&, int)
{
  return true;
}

void ExportPlugin::OptionsCreate(ShuttleGui &S, int WXUNUSED(format))
{
   S.StartHorizontalLay(wxCENTER);
   {
      S.StartHorizontalLay(wxCENTER, 0);
      {
         S.Prop(1).AddTitle(XO("No format specific options"));
      }
      S.EndHorizontalLay();
   }
   S.EndHorizontalLay();
}

TranslatableString ExportPluginEx::GetStatusString() const
{
   return mStatus;
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
