/*  SPDX-License-Identifier: GPL-2.0-or-later */

#pragma once

#include <QQmlEngine>

#include "GlobalVariable.h"

namespace audacity
{
   struct QT_INIT_API QMLEngineFactory final : GlobalHook<QMLEngineFactory, std::unique_ptr<QQmlEngine>()> { };
}
