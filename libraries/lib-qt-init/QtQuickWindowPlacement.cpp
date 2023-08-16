/*  SPDX-License-Identifier: GPL-2.0-or-later */

#include "QtQuickWindowPlacement.h"

QWindow* QtQuickWindowPlacement::Get(const WindowPlacement& placement)
{
   if(const auto qqplacement = dynamic_cast<const QtQuickWindowPlacement*>(&placement))
      return qqplacement->GetTarget();
   return nullptr;
}

QtQuickWindowPlacement::QtQuickWindowPlacement(QWindow* target)
   : mTarget(target)
{
}

QtQuickWindowPlacement::~QtQuickWindowPlacement() = default;

QtQuickWindowPlacement::operator bool() const
{
   return mTarget != nullptr;
}

QWindow* QtQuickWindowPlacement::GetTarget() const
{
   return mTarget;
}
