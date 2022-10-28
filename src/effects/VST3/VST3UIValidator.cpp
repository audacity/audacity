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
   if(!mWrapper.IsActive())
   {
      mAccess.ModifySettings([this](EffectSettings& settings)
      {
         mWrapper.FlushParameters(settings);
         return nullptr;
      });
   }
}


bool VST3UIValidator::TryLoadNativeUI(wxWindow* parent)
{
   using namespace Steinberg;

   if(const auto view = owned (mWrapper.mEditController->createView (Vst::ViewType::kEditor))) 
   {
      //Workaround: override default min size set by EffectUIHost::Initialize()
      parent->SetMinSize(wxDefaultSize);
      //IPlugFrame::resizeView is supposed to call IPlugView::setSize
      //in the same call stack, assign before frame is attached
      mPlugView = view;

#if __WXGTK__
      safenew internal::x11::SocketWindow(parent, wxID_ANY, view);
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
      mPlugFrame = plugFrame;

      ViewRect initialSize;
      if(view->getSize(&initialSize) == kResultOk)
         plugFrame->init(view.get(), &initialSize);
#endif
      return true;
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
      return nullptr;
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
      return nullptr;
   });
   //Make sure that new state has been written to the caches...
   mAccess.Flush();

   EffectUIValidator::OnClose();
}

bool VST3UIValidator::UpdateUI()
{
   mAccess.ModifySettings([&](EffectSettings& settings) {
      mWrapper.FetchSettings(settings);
      return nullptr;
   });
   
   if (mPlainUI != nullptr)
      mPlainUI->ReloadParameters();

   //Write to main...
   mAccess.Flush();

   return true;
}
