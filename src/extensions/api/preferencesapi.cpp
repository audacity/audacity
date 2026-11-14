/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 */
#include "preferencesapi.h"

#include "framework/global/settings.h"

#include "../extensionpreferences.h"

namespace au::extensions {
PreferencesApi::PreferencesApi(muse::api::IApiEngine* engine)
    : ApiObject(engine)
{
}

QVariant PreferencesApi::value(const QString& name, const QVariant& defaultValue) const
{
    const std::string scopeId = engine()->apiContext().scopeId;
    if (scopeId.empty()) {
        return defaultValue;
    }

    const muse::Val& stored = muse::settings()->value(extensionPreferenceKey(muse::Uri(scopeId), name.toStdString()));
    return stored.isNull() ? defaultValue : stored.toQVariant();
}
} // namespace au::extensions
