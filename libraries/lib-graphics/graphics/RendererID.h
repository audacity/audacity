/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  RendererID.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstddef>
#include <limits>
#include <string_view>

class GRAPHICS_API RendererID final
{
public:
   RendererID() = default;
   RendererID(const RendererID&) = default;
   RendererID(RendererID&&) = default;
   RendererID& operator =(const RendererID&) = default;
   RendererID& operator =(RendererID&&) = default;

   bool IsValid() const noexcept;
   std::string_view GetName() const noexcept;
   
   GRAPHICS_API friend bool
   operator==(const RendererID& lhs, const RendererID& rhs) noexcept;

   GRAPHICS_API friend bool
   operator!=(const RendererID& lhs, const RendererID& rhs) noexcept;

private:
   static constexpr size_t InvalidRenderer = std::numeric_limits<size_t>::max();

   RendererID(std::string_view name, size_t id);

   std::string_view mName;
   size_t mID { InvalidRenderer };

   GRAPHICS_API friend RendererID RegisterRenderer(std::string_view);
};

GRAPHICS_API RendererID GetRendererIndependentID();
GRAPHICS_API RendererID RegisterRenderer(std::string_view name);
