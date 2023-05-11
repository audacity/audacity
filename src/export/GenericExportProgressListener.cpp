/**********************************************************************

  Audacity: A Digital Audio Editor

  GenericExportProgressListener.cpp

  Vitaly Sverchinsky

**********************************************************************/

#include "GenericExportProgressListener.h"
#include "ExportPlugin.h"
#include "Internat.h"
#include "BasicUI.h"

GenericExportProgressListener::GenericExportProgressListener(ExportPlugin& plugin)
   : mPlugin(plugin)
{
   
}

GenericExportProgressListener::~GenericExportProgressListener() = default;

void GenericExportProgressListener::OnExportProgress(double value)
{
   constexpr long long ProgressSteps = 1000ul;
   
   if(!mProgressDialog)
   {
      mProgressDialog = BasicUI::MakeProgress(XO("Export"),
                                              mPlugin.GetStatusString());
   }
   else
      mProgressDialog->SetMessage(mPlugin.GetStatusString());
   
   const auto result = mProgressDialog->Poll(value * ProgressSteps, ProgressSteps);
   if(result == BasicUI::ProgressResult::Cancelled)
      mPlugin.Cancel();
   else if(result == BasicUI::ProgressResult::Stopped)
      mPlugin.Stop();
}

void GenericExportProgressListener::OnExportResult(ExportResult result)
{
   mResult = result;
   mProgressDialog.reset();
}

ExportProgressListener::ExportResult GenericExportProgressListener::ConsumeResult() noexcept
{
   auto result = ExportResult::Error;
   std::swap(result, mResult);
   return result;
}
