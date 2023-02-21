/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2Preferences.h
  @brief Persistent settings that can apply to any LV2 effect

  Paul Licameli split from LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/

#ifndef __AUDACITY_LV2_PREFERENCES__
#define __AUDACITY_LV2_PREFERENCES__

#if defined(USE_LV2)

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__clang__)
#pragma clang diagnostic ignored "-Wparentheses"
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include "../../widgets/wxPanelWrapper.h"

class EffectDefinitionInterface;
class ShuttleGui;


namespace LV2Preferences {
//! Maximum block size in number of samples (not bytes)
constexpr auto DEFAULT_BLOCKSIZE = 1048576;

/*! @name Persistent settings that can apply to any LV2 effect
 @{
 */
bool GetBufferSize(const EffectDefinitionInterface &effect, int &bufferSize);
bool SetBufferSize(const EffectDefinitionInterface &effect, int bufferSize);

bool GetUseLatency(const EffectDefinitionInterface &effect, bool &useLatency);
bool SetUseLatency(const EffectDefinitionInterface &effect, bool useLatency);

bool GetUseGUI(const EffectDefinitionInterface &effect, bool &useGUI);
bool SetUseGUI(const EffectDefinitionInterface &effect, bool useGUI);
/*!
 @}
*/

class Dialog final : public wxDialogWrapper
{
public:
   explicit Dialog(const EffectDefinitionInterface &effect);
   virtual ~Dialog();

   void PopulateOrExchange(ShuttleGui &S);

   void OnOk(wxCommandEvent &evt);

private:
   const EffectDefinitionInterface &mEffect;
   int mBufferSize{};
   bool mUseLatency{};
   bool mUseGUI{};

   DECLARE_EVENT_TABLE()
};
}

#endif
#endif
