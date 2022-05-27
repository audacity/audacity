/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  RendererID.h

  Dmitry Vedenko

**********************************************************************/
#include "RendererID.h"

#include <algorithm>
#include <string>
#include <deque>

namespace graphics
{

namespace
{
const auto independentId = RegisterRenderer("independent");
}

GRAPHICS_API RendererID GetRendererIndependentID()
{
   return independentId;
}

RendererID RegisterRenderer(std::string_view name)
{
   static std::deque<std::string> Renderers;

   auto it = std::find(Renderers.begin(), Renderers.end(), name);

   if (it != Renderers.end())
      return RendererID(*it, std::distance(Renderers.begin(), Renderers.end()));

   Renderers.emplace_back(name);

   return RendererID(Renderers.back(), Renderers.size() - 1);
}

bool operator==(const RendererID& lhs, const RendererID& rhs) noexcept
{
   return lhs.mID == rhs.mID;
}

bool operator!=(const RendererID& lhs, const RendererID& rhs) noexcept
{
   return !(lhs == rhs);
}

bool RendererID::IsValid() const noexcept
{
   return mID != InvalidRenderer;
}

std::string_view RendererID::GetName() const noexcept
{
   return mName;
}

RendererID::RendererID(std::string_view name, size_t id)
    : mName(name)
    , mID(id)
{
}

} // namespace graphics
