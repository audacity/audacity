/*  SPDX-License-Identifier: GPL-2.0-or-later */

#pragma once

#include "BasicUI.h"

class QWindow;

class QT_INIT_API QtQuickWindowPlacement final : public BasicUI::WindowPlacement
{
   QWindow* mTarget{};
public:
   static QWindow* Get(const WindowPlacement& placement);

   QtQuickWindowPlacement(QWindow* target = nullptr);
   ~QtQuickWindowPlacement() override;
   operator bool() const override;
   QWindow* GetTarget() const;
};
