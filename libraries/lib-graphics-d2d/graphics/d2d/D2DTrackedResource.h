/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DTrackedResource.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>

#include "Observer.h"

namespace graphics::d2d
{
   
class D2DRenderer;

class D2DTrackedResource /* not final */ :
    public std::enable_shared_from_this<D2DTrackedResource>
{
public:
   explicit D2DTrackedResource(D2DRenderer& renderer);
   virtual ~D2DTrackedResource() = default;

   template <typename T> std::shared_ptr<T> GetSharedPtr()
   {
      return std::static_pointer_cast<T>(shared_from_this());
   }

   template <typename T> std::shared_ptr<const T> GetSharedPtr() const
   {
      return std::static_pointer_cast<const T>(shared_from_this());
   }

   D2DRenderer& GetRenderer() noexcept;
   const D2DRenderer& GetRenderer() const noexcept;

protected:
   virtual void CleanupDirect2DResources() = 0;

   D2DRenderer& mRenderer;

private:
   Observer::Subscription mShutdownSubscription;

   friend class D2DRenderer;
};

} // namespace graphics::d2d
