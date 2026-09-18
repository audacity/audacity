/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QFileInfo>
#include <QString>

namespace au::importexport {
inline QString defaultExportFilename(const QString& displayName, const QString& projectTitle)
{
    // Never-saved projects have a session filename, but imports already set
    // their title to the source filename without its extension.
    if (displayName.isEmpty() || displayName.endsWith(QStringLiteral(".aup4unsaved"))) {
        return projectTitle.isEmpty() ? QStringLiteral("Untitled") : projectTitle;
    }

    return displayName;
}

inline QString exportFilenameWithExtension(const QString& filename, const QString& extension, bool isStem)
{
    // A generated name is a stem even when it contains dots (take.2.wav).
    // Keep the existing handling of explicitly entered filename extensions.
    if (!extension.isEmpty() && (isStem || QFileInfo(filename).suffix().isEmpty())) {
        return filename + QLatin1Char('.') + extension;
    }

    return filename;
}
}
