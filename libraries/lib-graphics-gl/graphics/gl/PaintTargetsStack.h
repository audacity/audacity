/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  PaintTargetStack.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>
#include <vector>

namespace graphics::gl
{
class GLRenderer;
class Context;
class Framebuffer;
using FramebufferPtr = std::shared_ptr<Framebuffer>;

class PaintTarget;

class PaintTargetsStack final
{
public:
   PaintTargetsStack(GLRenderer& renderer, Context& context);
   ~PaintTargetsStack();

   PaintTarget* PushTarget(const FramebufferPtr& framebuffer);
   PaintTarget* PopTarget();

private:
   GLRenderer& mRenderer;
   Context& mContext;

   std::vector<std::unique_ptr<PaintTarget>> mTargetsPool;
   std::vector<PaintTarget*> mFreeTargets;
   std::vector<PaintTarget*> mTargetsStack;
};
}
