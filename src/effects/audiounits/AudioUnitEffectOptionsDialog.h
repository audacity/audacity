/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitEffectOptionsDialog.h

  Dominic Mazzoni
  Leland Lucius

  Paul Licameli split from AudioUnitEffect.cpp

**********************************************************************/

#ifndef __AUDACITY_AUDIOUNIT_EFFECT_OPTIONS_DIALOG__
#define __AUDACITY_AUDIOUNIT_EFFECT_OPTIONS_DIALOG__

#include "wxPanelWrapper.h"

class ShuttleGui;

///////////////////////////////////////////////////////////////////////////////
//
// AudioUnitEffectOptionsDialog
//
///////////////////////////////////////////////////////////////////////////////

class AudioUnitEffectOptionsDialog final : public wxDialogWrapper
{
public:
   AudioUnitEffectOptionsDialog(
      wxWindow * parent, bool &useLatencey, wxString &uiType);
   virtual ~AudioUnitEffectOptionsDialog();
   void PopulateOrExchange(ShuttleGui & S);
   void OnOk(wxCommandEvent & evt);
private:
   bool &mUseLatency;
   wxString &mUIType;
   TranslatableString mUITypeString;
   DECLARE_EVENT_TABLE()
};

static const auto FullValue = XO("Full");
static const auto GenericValue = XO("Generic");
static const auto BasicValue = XO("Basic");

#endif
