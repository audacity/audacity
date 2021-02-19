/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PluginStartupRegistration.cpp

  @author Vitaly Sverchinsky

**********************************************************************/

#include "PluginStartupRegistration.h"

#include <thread>

#include <wx/log.h>
#include <wx/app.h>
#include <wx/stattext.h>
#include <wx/gauge.h>
#include <wx/button.h>
#include <wx/timer.h>
#include <wx/sizer.h>

#include "PluginManager.h"
#include "PluginDescriptor.h"
#include "wxPanelWrapper.h"

namespace
{
   enum {
      OnPluginScanTimeout = wxID_HIGHEST + 1,
   };

   class PluginScanDialog : public wxDialogWrapper
   {
      wxStaticText* mText{nullptr};
      wxStaticText* mElapsed{nullptr};
      wxGauge* mProgress{nullptr};
      std::chrono::system_clock::time_point mStartTime;
   public:
      
      PluginScanDialog(
         wxWindow* parent,
         wxWindowID winid,
         const TranslatableString& title)
         : wxDialogWrapper(parent, winid, title)
      {
         auto rootSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
         auto topSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
         topSizer->Add(mText =
            safenew wxStaticText(
               this,
               wxID_ANY,
               wxEmptyString,
               wxDefaultPosition,
               wxDefaultSize,
               wxST_ELLIPSIZE_START | wxST_NO_AUTORESIZE),
            1, wxEXPAND);
         topSizer->AddSpacer(5);
         topSizer->Add(safenew wxButton(this, wxID_IGNORE, _("&Skip")), 0);

         auto timerSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
         timerSizer->Add(
            safenew wxStaticText(
               this,
               wxID_ANY,
               _("Elapsed Time:"),
               wxDefaultPosition,
               wxDefaultSize,
               wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL), 1, wxEXPAND);
         timerSizer->AddSpacer(5);
         timerSizer->Add(mElapsed =
            safenew wxStaticText(
               this,
               wxID_ANY,
               wxEmptyString,
               wxDefaultPosition,
               wxDefaultSize,
               wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL), 1, wxEXPAND);
         
         rootSizer->Add(topSizer.release(), 0, wxEXPAND | wxALL, 10);
         rootSizer->Add(mProgress =
            safenew wxGauge(this, wxID_ANY, 1000),
            0, wxEXPAND | wxLEFT | wxRIGHT, 10);
         rootSizer->AddSpacer(10);
         rootSizer->Add(timerSizer.release(), 0, wxEXPAND);
#ifdef __WXMAC__
         rootSizer->Add(CreateButtonSizer(wxCANCEL), 0, wxEXPAND);
#else
         rootSizer->Add(CreateButtonSizer(wxCANCEL), 0, wxEXPAND | wxALL, 10);
#endif    
         SetSizer(rootSizer.release());

         Bind(wxEVT_SHOW, &PluginScanDialog::OnShow, this);
         Bind(wxEVT_IDLE, &PluginScanDialog::OnIdle, this);

         SetInitialSize({500, -1});
      }

      void UpdateProgress(const wxString& text, float progress)
      {
         mText->SetLabel(text);
         mText->SetToolTip(text);
         mProgress->SetValue(mProgress->GetRange() * progress);
      }

      void OnShow(wxShowEvent& evt)
      {
         evt.Skip();
         if(evt.IsShown())
         {
            mStartTime = std::chrono::system_clock::now();
            UpdateElapsedTime();
         }
      }
      
      void OnIdle(wxIdleEvent& evt)
      {
         evt.Skip();
         UpdateElapsedTime();
      }

      void UpdateElapsedTime()
      {
         auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now() - mStartTime).count();
         mElapsed->SetLabel(wxTimeSpan(0, 0, 0, elapsed).Format("%H:%M:%S"));
      }
   };
}

PluginStartupRegistration::PluginStartupRegistration(const std::map<wxString, std::vector<wxString>>& pluginsToProcess)
{
   for(auto& p : pluginsToProcess)
      mPluginsToProcess.push_back(p);
}

void PluginStartupRegistration::OnInternalError(const wxString& error)
{
   StopWithError(error);
}

void PluginStartupRegistration::OnPluginFound(const PluginDescriptor& desc)
{
   if(!mValidProviderFound)
      mFailedPluginsCache.clear();

   mValidProviderFound = true;
   if(!desc.IsValid())
      mFailedPluginsCache.push_back(desc);
   PluginManager::Get().RegisterPlugin(PluginDescriptor { desc });
}

void PluginStartupRegistration::OnPluginValidationFailed(const wxString& providerId, const wxString& path)
{
   PluginID ID = providerId + wxT("_") + path;
   PluginDescriptor pluginDescriptor;
   pluginDescriptor.SetPluginType(PluginTypeStub);
   pluginDescriptor.SetID(ID);
   pluginDescriptor.SetProviderID(providerId);
   pluginDescriptor.SetPath(path);
   pluginDescriptor.SetEnabled(false);
   pluginDescriptor.SetValid(false);

   //Multiple providers can report same module paths
   //do not register until all associated providers have tried to load the module
   mFailedPluginsCache.push_back(std::move(pluginDescriptor));
}


void PluginStartupRegistration::OnValidationFinished()
{
   ++mCurrentPluginProviderIndex;
   if(mValidProviderFound ||
      mPluginsToProcess[mCurrentPluginIndex].second.size() == mCurrentPluginProviderIndex)
   {
      if(!mFailedPluginsCache.empty())
      {
         //we've tried all providers associated with same module path...
         if(!mValidProviderFound)
         {
            //...but none of them succeeded
            mFailedPluginsPaths.push_back(mFailedPluginsCache[0].GetPath());

            //Same plugin path, but different providers, we need to register all of them
            for(auto& desc : mFailedPluginsCache)
               PluginManager::Get().RegisterPlugin(std::move(desc));
         }
         //plugin type was detected, but plugin instance validation has failed
         else
         {
            for(auto& desc : mFailedPluginsCache)
            {
               if(desc.GetPluginType() != PluginTypeStub)
                  mFailedPluginsPaths.push_back(desc.GetPath());
            }
         }
      }
      ++mCurrentPluginIndex;
      mCurrentPluginProviderIndex = 0;
      mValidProviderFound = false;
      mFailedPluginsCache.clear();
   }
   ProcessNext();
}

const std::vector<wxString>& PluginStartupRegistration::GetFailedPluginsPaths() const noexcept
{
   return mFailedPluginsPaths;
}

void PluginStartupRegistration::Run(std::chrono::seconds timeout)
{
   PluginScanDialog dialog(nullptr, wxID_ANY, XO("Searching for plugins"));
   wxTimer timeoutTimer(&dialog, OnPluginScanTimeout);
   mScanDialog = &dialog;
   mTimeoutTimer = &timeoutTimer;
   mTimeout = timeout;

   dialog.Bind(wxEVT_BUTTON, [this](wxCommandEvent& evt) {
      evt.Skip();
      if(evt.GetId() == wxID_IGNORE)
         Skip();
   });
   dialog.Bind(wxEVT_TIMER, [this](wxTimerEvent& evt) {
      if(evt.GetId() == OnPluginScanTimeout)
      {
         if(mValidator && mValidator->InactiveSince() < mRequestStartTime)
            Skip();
         //else
         //   wxMessageBox("Please check for plugin popups!");
      }
      else
         evt.Skip();
   });
   dialog.Bind(wxEVT_CLOSE_WINDOW, [this](wxCloseEvent& evt) {
      evt.Skip();
      mValidator.reset();
      PluginManager::Get().Save();
   });

   dialog.CenterOnScreen();
   ProcessNext();
   dialog.ShowModal();
}

void PluginStartupRegistration::Stop()
{
   if(auto dialog = mScanDialog.get())
      dialog->Close();
}

void PluginStartupRegistration::Skip()
{
   //Drop current validator, no more callbacks will be received from now
   mValidator->SetDelegate(nullptr);
   //While on Linux and MacOS socket `shutdown()` wakes up `select()` almost
   //immediately, on Windows it sometimes get delayed on unspecified amount
   //of time. As we do not expect any data we can safely move remaining
   //operations to another thread.
   std::thread([validator = std::shared_ptr<AsyncPluginValidator>(std::move(mValidator))]{ }).detach();

   if(!mValidProviderFound)
   {
      // Validator didn't report anything yet or it tried
      // one or more providers that didn't recognize the plugin.
      // In that case we assume that none of the remaining providers
      // can recognize that plugin.
      // Note: create stub `PluginDescriptors` for each associated provider
      for(;mCurrentPluginProviderIndex < mPluginsToProcess[mCurrentPluginIndex].second.size(); ++mCurrentPluginProviderIndex)
         OnPluginValidationFailed(
            mPluginsToProcess[mCurrentPluginIndex].second[mCurrentPluginProviderIndex],
            mPluginsToProcess[mCurrentPluginIndex].first);
      mCurrentPluginProviderIndex = mPluginsToProcess[mCurrentPluginIndex].second.size() - 1;
   }
   //else
   //    Don't assume that `OnValidationFinished()` and `OnPluginFound()`
   //    aren't deferred within run loop

   OnValidationFinished();
}

void PluginStartupRegistration::StopWithError(const wxString& msg)
{
   //TODO: show error dialog?
   wxLogError("Plugin registration error: %s", msg);
   Stop();
}

void PluginStartupRegistration::ProcessNext()
{
   if(mCurrentPluginIndex == mPluginsToProcess.size())
   {
      Stop();
      return;
   }

   try
   {
      if(auto dialog = static_cast<PluginScanDialog*>(mScanDialog.get()))
      {
         const auto progress = static_cast<float>(mCurrentPluginIndex) / static_cast<float>(mPluginsToProcess.size());
         dialog->UpdateProgress(
            mPluginsToProcess[mCurrentPluginIndex].first,
            progress);
      }
      if(!mValidator)
         mValidator = std::make_unique<AsyncPluginValidator>(*this);

      mValidator->Validate(
         mPluginsToProcess[mCurrentPluginIndex].second[mCurrentPluginProviderIndex],
         mPluginsToProcess[mCurrentPluginIndex].first
      );
      mRequestStartTime = std::chrono::system_clock::now();
      if(auto timer = mTimeoutTimer.get())
         timer->StartOnce(std::chrono::duration_cast<std::chrono::milliseconds>(mTimeout).count());
   }
   catch(std::exception& e)
   {
      StopWithError(e.what());
   }
   catch(...)
   {
      StopWithError("unknown error");
   }
}

