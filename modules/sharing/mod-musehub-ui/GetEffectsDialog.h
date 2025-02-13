/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  GetEffectsDialog.h

  Dmitry Makarenko

**********************************************************************/
#pragma once

#include <wx/treebook.h>

#include "RoundedStaticBitmap.h"
#include "wxPanelWrapper.h"
#include "MuseHubService.h"

class RoundedStaticBitmap;

namespace audacity::musehub
{

class MUSEHUB_UI_API GetEffectsDialog final : public wxDialogWrapper
{
public:
   GetEffectsDialog(wxWindow *parent = nullptr);
   ~GetEffectsDialog() override = default;

private:
   wxTreebook* m_treebook;

   void AddEffectsPage(const std::string& group, const std::vector<EffectInfo>& effects);
   void AddBecomeAPartnerPage();
   void AddLoadingPage();
   void AddLoadingErrorPage();

   void ReloadEffectList();

   void FetchImage(RoundedStaticBitmap* bitmap, const std::string& url);
};

}
