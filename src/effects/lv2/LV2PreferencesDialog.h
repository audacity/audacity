/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2PreferencesDialog.h
  @brief UI to change persistent settings that can apply to any LV2 effect

  Paul Licameli split from LV2Preferences.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/

#ifndef __AUDACITY_LV2_PREFERENCES_DIALOG__
#define __AUDACITY_LV2_PREFERENCES_DIALOG__

#if defined(USE_LV2)

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__clang__)
#pragma clang diagnostic ignored "-Wparentheses"
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include "wxPanelWrapper.h"

class EffectDefinitionInterface;
class ShuttleGui;


class LV2PreferencesDialog final : public wxDialogWrapper
{
public:
   explicit LV2PreferencesDialog(const EffectDefinitionInterface &effect);
   virtual ~LV2PreferencesDialog();

   void PopulateOrExchange(ShuttleGui &S);

   void OnOk(wxCommandEvent &evt);

private:
   const EffectDefinitionInterface &mEffect;
   int mBufferSize{};
   bool mUseLatency{};
   bool mUseGUI{};

   DECLARE_EVENT_TABLE()
};

#endif
#endif
