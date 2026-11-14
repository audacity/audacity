/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 */
#pragma once

#include <QString>
#include <QVariant>

#include "framework/global/api/apiobject.h"

namespace au::extensions {
class PreferencesApi final : public muse::api::ApiObject
{
    Q_OBJECT

public:
    explicit PreferencesApi(muse::api::IApiEngine* engine);

    Q_INVOKABLE QVariant value(const QString& name, const QVariant& defaultValue = {}) const;
};
} // namespace au::extensions
