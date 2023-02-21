/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3OptionsDialog.h

  @brief Part of Audacity VST3 module, duplicate from VSTEffect.cpp 

**********************************************************************/

#pragma once

#include "widgets/wxPanelWrapper.h"

class EffectDefinitionInterface;
class ShuttleGui;

class VST3OptionsDialog final : public wxDialogWrapper
{
public:
   explicit VST3OptionsDialog(const EffectDefinitionInterface &effect);
   virtual ~VST3OptionsDialog();

   void PopulateOrExchange(ShuttleGui & S);

   void OnOk(wxCommandEvent & evt);

private:
   const EffectDefinitionInterface &mEffect;
   int mBufferSize;
   bool mUseGUI;
   bool mUseLatency;

   DECLARE_EVENT_TABLE()
};
