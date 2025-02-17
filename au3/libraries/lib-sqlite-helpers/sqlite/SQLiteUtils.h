/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: SQLiteUtils.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <string_view>
#include <functional>

#include "Error.h"

namespace audacity::sqlite {
SQLITE_HELPERS_API Error Initialize() noexcept;

using LogCallback = std::function<void (int, std::string_view)>;
SQLITE_HELPERS_API void SetLogCallback(LogCallback callback);
} // namespace audacity::sqlite
