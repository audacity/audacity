/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QFileInfo>
#include <QStringList>

namespace au::importexport {
inline QString defaultExportFilename(const QString& displayName, const QString& projectTitle)
{
    if (displayName.isEmpty() || displayName.endsWith(QStringLiteral(".aup4unsaved"))) {
        return projectTitle.isEmpty() ? QStringLiteral("Untitled") : projectTitle;
    }
    return displayName;
}

inline QString exportFilenameWithExtensions(const QString& filename, const QStringList& extensions, bool isStem)
{
    // Imported titles are stems, even when their last component looks like an extension.
    // Explicit names retain the selected format's recognized extensions.
    if (!extensions.isEmpty()
        && (isStem || !extensions.contains(QFileInfo(filename).suffix(), Qt::CaseInsensitive))) {
        return filename + QLatin1Char('.') + extensions.front();
    }
    return filename;
}
}
