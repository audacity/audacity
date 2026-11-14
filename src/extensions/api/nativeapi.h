/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 */
#pragma once

#include <QObject>
#include <QString>

#include "framework/global/api/apiobject.h"

namespace au::extensions {
class NativeApi final : public muse::api::ApiObject
{
    Q_OBJECT

public:
    explicit NativeApi(muse::api::IApiEngine* engine);

    Q_INVOKABLE QObject* open(const QString& name);
    Q_INVOKABLE QObject* createBuffer(qulonglong byteLength);
};
} // namespace au::extensions
