/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MuseHubService.cpp

  Dmitry Makarenko

**********************************************************************/
#pragma once

#include <functional>
#include <string>
#include <vector>

namespace audacity::musehub
{

struct EffectInfo {
   std::string iconUrl;
   std::string code;
   std::string title;
   std::string subtitle;
   std::string category;
};

struct EffectsGroup {
   std::string title;
   std::vector<EffectInfo> effects;
};

std::string MUSEHUB_API GetBecomeAPartnerUrl();
std::string MUSEHUB_API GetMusehubAPIEndpoint();
std::string MUSEHUB_API GetEffectUrl(const std::string& effectCode);

void MUSEHUB_API GetEffects(std::function<void(std::vector<EffectsGroup>)> callback);

}
