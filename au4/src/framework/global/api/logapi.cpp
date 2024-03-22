/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include "logapi.h"

#include "log.h"

using namespace mu::api;

LogApi::LogApi(api::IApiEngine* e)
    : ApiObject(e)
{
}

void LogApi::error(const QString& message)
{
    error("Api", message);
}

void LogApi::warn(const QString& message)
{
    warn("Api", message);
}

void LogApi::info(const QString& message)
{
    info("Api", message);
}

void LogApi::debug(const QString& message)
{
    debug("Api", message);
}

void LogApi::error(const QString& tag, const QString& message)
{
    LOGE_T(tag.toStdString())() << message;
}

void LogApi::warn(const QString& tag, const QString& message)
{
    LOGW_T(tag.toStdString())() << message;
}

void LogApi::info(const QString& tag, const QString& message)
{
    LOGI_T(tag.toStdString())() << message;
}

void LogApi::debug(const QString& tag, const QString& message)
{
    LOGD_T(tag.toStdString())() << message;
}
