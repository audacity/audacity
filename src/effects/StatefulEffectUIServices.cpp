/**********************************************************************

  Audacity: A Digital Audio Editor

 StatefulEffectUIServices.cpp

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from Effect.cpp

**********************************************************************/
#include "StatefulEffectUIServices.h"
#include "EffectUIServices.h"
#include "StatefulEffectBase.h"
#include <wx/window.h>

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
