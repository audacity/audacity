/**********************************************************************

  Audacity: A Digital Audio Editor

  StatefulEffectUIServices.h

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from Effect.h

**********************************************************************/
#ifndef __AUDACITY_STATEFUL_EFFECT_UI_SERVICES__
#define __AUDACITY_STATEFUL_EFFECT_UI_SERVICES__

#include "EffectEditor.h"

class EffectPlugin;
class StatefulEffectBase;

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

#endif
