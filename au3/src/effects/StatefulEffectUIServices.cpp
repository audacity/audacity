/**********************************************************************

  Audacity: A Digital Audio Editor

 StatefulEffectUIServices.cpp

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from Effect.cpp

**********************************************************************/
#include "StatefulEffectUIServices.h"
#include "EffectEditor.h"
#include "ShuttleGui.h"
#include <wx/sizer.h>
#include <wx/window.h>

namespace {
//! Default implementation of EffectEditor invokes ValidateUI
//! method of an EffectUIServices
/*
 Also pops the even handler stack of a window, if given to the contructor

 This is a transitional class; it should be eliminated when all effect classes
 define their own associated subclasses of EffectEditor, which can hold
 state only for the lifetime of a dialog, so the effect object need not hold it
*/
class DefaultEffectEditor
   : public EffectEditor
   // Inherit wxEvtHandler so that Un-Bind()-ing is automatic in the destructor
   , protected wxEvtHandler
{
public:
   /*!
    @param pParent if not null, caller will push an event handler onto this
    window; then this object is responsible to pop it
    */
   DefaultEffectEditor(const EffectPlugin &plugin,
      EffectUIServices &services, EffectSettingsAccess &access,
      wxWindow *pParent = nullptr);
   //! Calls Disconnect
   ~DefaultEffectEditor() override;
   //! Calls mServices.ValidateUI()
   bool ValidateUI() override;
   void Disconnect() override;
protected:
   const EffectPlugin &mPlugin;
   wxWindow *mpParent{};
};

DefaultEffectEditor::DefaultEffectEditor(const EffectPlugin &plugin,
   EffectUIServices &services, EffectSettingsAccess &access,
   wxWindow *pParent
)  : EffectEditor{ services, access }
   , mPlugin{ plugin }
   , mpParent{ pParent }
{
}

DefaultEffectEditor::~DefaultEffectEditor()
{
   Disconnect();
}

bool DefaultEffectEditor::ValidateUI()
{
   bool result {};
   mAccess.ModifySettings([&](EffectSettings &settings){
      result = mUIServices.ValidateUI(mPlugin, settings);
      return nullptr;
   });
   return result;
}

void DefaultEffectEditor::Disconnect()
{
   if (mpParent) {
      mpParent->PopEventHandler();
      mpParent = nullptr;
   }
}
}

StatefulEffectUIServices::~StatefulEffectUIServices() = default;

std::unique_ptr<EffectEditor>
StatefulEffectUIServices::PopulateUI(const EffectPlugin &plugin, ShuttleGui &S,
   EffectInstance &instance, EffectSettingsAccess &access,
   const EffectOutputs *pOutputs) const
{
   auto parent = S.GetParent();

   // As in MakeInstance, we still cheat const for stateful effects!
   auto pThis = const_cast<StatefulEffectUIServices*>(this);

   // Let the effect subclass provide its own editor if it wants
   auto result = pThis->PopulateOrExchange(S, instance, access, pOutputs);

   parent->SetMinSize(parent->GetSizer()->GetMinSize());

   if (!result) {
      // No custom editor object?  Then use the default
      result = std::make_unique<DefaultEffectEditor>(plugin,
         *pThis, access, S.GetParent());
      parent->PushEventHandler(pThis);
   }
   return result;
}

std::unique_ptr<EffectEditor> StatefulEffectUIServices::PopulateOrExchange(
   ShuttleGui &, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *)
{
   return nullptr;
}

bool StatefulEffectUIServices::TransferDataToWindow(const EffectSettings &)
{
   return true;
}

bool StatefulEffectUIServices::TransferDataFromWindow(EffectSettings &)
{
   return true;
}
