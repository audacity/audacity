/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 */
#pragma once

#include <QJSValue>
#include <QObject>
#include <QString>

#include "framework/global/api/apiobject.h"

namespace au::extensions {
class AudioTransformApi final : public muse::api::ApiObject
{
    Q_OBJECT

public:
    explicit AudioTransformApi(muse::api::IApiEngine* engine);

    Q_INVOKABLE QObject* create(QObject* library, const QString& operation, const QJSValue& contexts);
};
} // namespace au::extensions
