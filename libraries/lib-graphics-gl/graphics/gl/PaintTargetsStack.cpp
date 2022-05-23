/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  PaintTargetStack.cpp

  Dmitry Vedenko

**********************************************************************/
#include "PaintTargetsStack.h"
#include "PaintTarget.h"

namespace graphics::gl
{
PaintTargetsStack::PaintTargetsStack(GLRenderer& renderer, Context& context)
    : mRenderer(renderer)
    , mContext(context)
{
}
PaintTargetsStack::~PaintTargetsStack()
{
}
PaintTarget* PaintTargetsStack::PushTarget(const FramebufferPtr& framebuffer)
{
   PaintTarget* target = nullptr;

   if (!mFreeTargets.empty())
   {
      target = mFreeTargets.back();
      mFreeTargets.pop_back();
   }
   else
   {
      mTargetsPool.emplace_back(new PaintTarget(mRenderer, mContext));
      target = mTargetsPool.back().get();
   }

   mTargetsStack.push_back(target);

   target->BeginRendering(framebuffer);

   return target;
}
PaintTarget* PaintTargetsStack::PopTarget()
{
   assert(!mTargetsStack.empty());

   if (mTargetsStack.empty())
      return nullptr;

   auto target = mTargetsStack.back();
   mTargetsStack.pop_back();

   target->EndRendering();

   mFreeTargets.push_back(target);

   if (mTargetsStack.empty())
      return nullptr;

   mTargetsStack.back()->RestartRendering();

   return mTargetsStack.back();
}
} // namespace graphics::gl
