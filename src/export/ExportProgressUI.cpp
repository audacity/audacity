/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportProgressUI.cpp

  Vitaly Sverchinsky

**********************************************************************/

#include "ExportProgressUI.h"

#include "ExportPlugin.h"
#include "Internat.h"
#include "BasicUI.h"
#include "AudacityMessageBox.h"

namespace
{
   class DialogExportProgressDelegate : public ExportPluginDelegate
   {
      bool mCancelled {false};
      bool mStopped {false};

      TranslatableString mStatus;
      TranslatableString mError;

      std::unique_ptr<BasicUI::ProgressDialog> mProgressDialog;
   public:

      bool IsCancelled() const override
      {
         return mCancelled;
      }

      bool IsStopped() const override
      {
         return mStopped;
      }

      void SetErrorString(const TranslatableString& str) override
      {
         mError = str;
      }

      void SetStatusString(const TranslatableString& str) override
      {
         mStatus = str;
      }

      void OnProgress(double progress) override
      {
         constexpr long long ProgressSteps = 1000ul;

         if(!mProgressDialog)
            mProgressDialog = BasicUI::MakeProgress(XO("Export"), mStatus);
         else
            mProgressDialog->SetMessage(mStatus);
         
         const auto result = mProgressDialog->Poll(progress * ProgressSteps, ProgressSteps);

         if(result == BasicUI::ProgressResult::Cancelled)
         {
            if(!mStopped)
               mCancelled = true;
         }
         else if(result == BasicUI::ProgressResult::Stopped)
         {
            if(!mCancelled)
               mStopped = true;
         }
      }

      const TranslatableString& GetErrorString() const noexcept
      {
         return mError;
      }
   };

}

ExportResult ExportProgressUI::Show(ExportTask exportTask)
{
   auto f = exportTask.get_future();

   DialogExportProgressDelegate delegate;
   exportTask(delegate);

   const auto result = f.get();
   if(result == ExportResult::Error)
   {
      if(!delegate.GetErrorString().empty())
         AudacityMessageBox(delegate.GetErrorString(),
                            XO("Error"),
                            wxOK | wxCENTRE | wxICON_EXCLAMATION);
   }
   return result;
}
