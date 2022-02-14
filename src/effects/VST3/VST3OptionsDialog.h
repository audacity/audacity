/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3OptionsDialog.h

  @brief Part of Audacity VST3 module, duplicate from VSTEffect.cpp 

**********************************************************************/

#pragma once

#include "widgets/wxPanelWrapper.h"
#include "EffectHostInterface.h"

class ShuttleGui;

class VST3OptionsDialog final : public wxDialogWrapper
{
public:
   VST3OptionsDialog(wxWindow * parent,
      EffectHostInterface &host, EffectDefinitionInterface &effect);
   virtual ~VST3OptionsDialog();

   void PopulateOrExchange(ShuttleGui & S);

   void OnOk(wxCommandEvent & evt);

private:
   EffectHostInterface &mHost;
   EffectDefinitionInterface &mEffect;
   int mBufferSize;
   bool mUseGUI;
   bool mUseLatency;

   DECLARE_EVENT_TABLE()
};
