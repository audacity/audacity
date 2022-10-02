#include "VST3UIValidator.h"

#include <pluginterfaces/gui/iplugview.h>

#include "VST3ParametersWindow.h"
#include "VST3Utils.h"
#include "VST3Wrapper.h"
#include "effects/EffectBase.h"
#include "internal/PlugFrame.h"
#include "widgets/NumericTextCtrl.h"

#ifdef __WXGTK__
#include "internal/x11/SocketWindow.h"
#endif

VST3UIValidator::VST3UIValidator(wxWindow* parent, VST3Wrapper& wrapper, EffectBase& effect, EffectSettingsAccess& access, bool useNativeUI)
   : EffectUIValidator(effect, access), mWrapper(wrapper), mParent(parent)
{
   if(effect.GetType() == EffectTypeGenerate)
   {
      auto vSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
      auto controlsRoot = safenew wxWindow(parent, wxID_ANY);
      if(!useNativeUI || !TryLoadNativeUI(controlsRoot))
         mPlainUI = VST3ParametersWindow::Setup(*controlsRoot, *mWrapper.mEditController, *mWrapper.mComponentHandler);
      vSizer->Add(controlsRoot);

      auto &extra = access.Get().extra;
      mDuration = safenew NumericTextCtrl(
            parent, wxID_ANY,
            NumericConverter::TIME,
            extra.GetDurationFormat(),
            extra.GetDuration(),
            mWrapper.mSetup.sampleRate,
            NumericTextCtrl::Options{}
               .AutoPos(true)
         );
      mDuration->SetName( XO("Duration") );

      auto hSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
      hSizer->Add(safenew wxStaticText(parent, wxID_ANY, _("Duration:")));
      hSizer->AddSpacer(5);
      hSizer->Add(mDuration);
      vSizer->AddSpacer(10);
      vSizer->Add(hSizer.release());

      parent->SetMinSize(vSizer->CalcMin());
      parent->SetSizer(vSizer.release());
   }
   else if(!useNativeUI || !TryLoadNativeUI(parent))
   {
      mPlainUI = VST3ParametersWindow::Setup(
         *parent,
         *mWrapper.mEditController,
         *mWrapper.mComponentHandler
      );
   }
   mWrapper.BeginParameterEdit(mAccess);

   Bind(wxEVT_IDLE, &VST3UIValidator::OnIdle, this);

   mParent->PushEventHandler(this);
}

VST3UIValidator::~VST3UIValidator() = default;

void VST3UIValidator::OnIdle(wxIdleEvent& evt)
{
   evt.Skip();
   mAccess.ModifySettings([this](EffectSettings& settings)
   {
      if(!mWrapper.IsActive())
         mWrapper.FlushParameters(settings);
   });
}


bool VST3UIValidator::TryLoadNativeUI(wxWindow* parent)
{
   using namespace Steinberg;

   if(const auto view = owned (mWrapper.mEditController->createView (Vst::ViewType::kEditor))) 
   {  
      parent->Bind(wxEVT_SIZE, &VST3UIValidator::OnEffectWindowResize, this);

      ViewRect defaultSize;
      if(view->getSize(&defaultSize) == kResultOk)
      {
         if(view->canResize() == Steinberg::kResultTrue)
         {
            ViewRect minSize {0, 0, parent->GetMinWidth(), parent->GetMinHeight()};
            ViewRect maxSize {0, 0, 10000, 10000};
            if(view->checkSizeConstraint(&minSize) != kResultOk)
               minSize = defaultSize;

            if(view->checkSizeConstraint(&maxSize) != kResultOk)
               maxSize = defaultSize;

            //No need to accommodate the off-by-one error with Steinberg::ViewRect
            //as we do it with wxRect

            if(defaultSize.getWidth() < minSize.getWidth())
               defaultSize.right = defaultSize.left + minSize.getWidth();
            if(defaultSize.getWidth() > maxSize.getWidth())
               defaultSize.right = defaultSize.left + maxSize.getWidth();

            if(defaultSize.getHeight() < minSize.getHeight())
               defaultSize.bottom = defaultSize.top + minSize.getHeight();
            if(defaultSize.getHeight() > maxSize.getHeight())
               defaultSize.bottom = defaultSize.top + maxSize.getHeight();

            parent->SetMinSize({minSize.getWidth(), minSize.getHeight()});
            parent->SetMaxSize({maxSize.getWidth(), maxSize.getHeight()});
         }
         else
         {
            parent->SetMinSize({defaultSize.getWidth(), defaultSize.getHeight()});
            parent->SetMaxSize(parent->GetMinSize());
         }

         parent->SetSize({defaultSize.getWidth(), defaultSize.getHeight()});
      }

#if __WXGTK__
      mPlugView = view;
      safenew internal::x11::SocketWindow(parent, wxID_ANY, view);
      return true;
#else

      static const auto platformType =
#  if __WXMAC__
         kPlatformTypeNSView;
#  elif __WXMSW__
         kPlatformTypeHWND;
#  else
#     error "Platform not supported"
#  endif
      auto plugFrame = owned(safenew internal::PlugFrame { parent });
      view->setFrame(plugFrame);
      if(view->attached(parent->GetHandle(), platformType) != kResultOk)
         return false;

      mPlugView = view;
      mPlugFrame = plugFrame;

      return true;
#endif
   }
   return false;
}

bool VST3UIValidator::IsGraphicalUI()
{
   return mPlugView != nullptr;
}

bool VST3UIValidator::ValidateUI()
{
   mAccess.ModifySettings([&](EffectSettings &settings){
      if (mDuration != nullptr)
         settings.extra.SetDuration(mDuration->GetValue());
      mWrapper.FlushParameters(settings);
      mWrapper.StoreSettings(settings);
   });

   return true;
}

void VST3UIValidator::OnClose()
{
   using namespace Steinberg;

   mParent->PopEventHandler();

   mPlainUI = nullptr;
   mParent = nullptr;
   if(mPlugView)
   {
      mPlugView->setFrame(nullptr);
      mPlugView->removed();
      mPlugView = nullptr;
      mPlugFrame = nullptr;
   }

   mWrapper.EndParameterEdit();
   
   mAccess.ModifySettings([&](EffectSettings &settings){
      if (mDuration != nullptr)
         settings.extra.SetDuration(mDuration->GetValue());
      //Flush changes if there is no processing performed at the moment
      mWrapper.FlushParameters(settings);
      mWrapper.StoreSettings(settings);
   });
   //Make sure that new state has been written to the caches...
   mAccess.Flush();

   EffectUIValidator::OnClose();
}

bool VST3UIValidator::UpdateUI()
{
   mAccess.ModifySettings([&](EffectSettings& settings) { mWrapper.FetchSettings(settings); });
   
   if (mPlainUI != nullptr)
      mPlainUI->ReloadParameters();

   //Write to main...
   mAccess.Flush();

   return true;
}

void VST3UIValidator::OnEffectWindowResize(wxSizeEvent& evt)
{
   using namespace Steinberg;

   if(!mPlugView)
      return;

   const auto window = static_cast<wxWindow*>(evt.GetEventObject());
   const auto windowSize = evt.GetSize();

   {
      //Workaround to prevent dialog window resize when
      //plugin window reaches its maximum size
      auto root = wxGetTopLevelParent(window);
      wxSize maxRootSize = root->GetMaxSize();

      //remember the current dialog size as its new maximum size
      if(window->GetMaxWidth() != -1 && windowSize.GetWidth() >= window->GetMaxWidth())
         maxRootSize.SetWidth(root->GetSize().GetWidth());
      if(window->GetMaxHeight() != -1 && windowSize.GetHeight() >= window->GetMaxHeight())
         maxRootSize.SetHeight(root->GetSize().GetHeight());
      root->SetMaxSize(maxRootSize);
   }
   
   ViewRect plugViewSize { 0, 0, windowSize.x, windowSize.y };
   mPlugView->checkSizeConstraint(&plugViewSize);
   mPlugView->onSize(&plugViewSize);
}
