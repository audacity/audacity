/**********************************************************************

  Audacity: A Digital Audio Editor

  PlatformCompatibility.cpp

  Qt-based implementation. Requires QCoreApplication to be initialized.
  This replaces the wxWidgets-based PlatformCompatibilityWX.cpp.

**********************************************************************/

#include "PlatformCompatibility.h"
#include <QStandardPaths>
#include <QCoreApplication>
#include <QDir>
#include <QFileInfo>

std::string PlatformCompatibility::GetUserDataDir()
{
    return QStandardPaths::writableLocation(QStandardPaths::AppDataLocation).toStdString();
}

std::string PlatformCompatibility::GetUserLocalDataDir()
{
    return QStandardPaths::writableLocation(QStandardPaths::AppLocalDataLocation).toStdString();
}

std::string PlatformCompatibility::GetResourcesDir()
{
#ifdef __APPLE__
    // On macOS, resources are in Contents/Resources
    QDir dir(QCoreApplication::applicationDirPath());
    dir.cdUp(); // Go to Contents
    return dir.filePath("Resources").toStdString();
#else
    // On other platforms, use the data directory
    return GetDataDir();
#endif
}

std::string PlatformCompatibility::GetDataDir()
{
    return QStandardPaths::writableLocation(QStandardPaths::AppDataLocation).toStdString();
}

std::string PlatformCompatibility::GetPluginsDir()
{
#ifdef __APPLE__
    // On macOS, plugins are in Contents/Plugins
    QDir dir(QCoreApplication::applicationDirPath());
    dir.cdUp(); // Go to Contents
    return dir.filePath("Plugins").toStdString();
#else
    // On other platforms, plugins are in the application directory
    return QCoreApplication::applicationDirPath().toStdString();
#endif
}

std::string PlatformCompatibility::GetDocumentsDir()
{
    return QStandardPaths::writableLocation(QStandardPaths::DocumentsLocation).toStdString();
}

std::string PlatformCompatibility::GetExecutablePath()
{
    return QCoreApplication::applicationFilePath().toStdString();
}

std::string PlatformCompatibility::GetTempDir()
{
    return QStandardPaths::writableLocation(QStandardPaths::TempLocation).toStdString();
}

std::string PlatformCompatibility::GetHomeDir()
{
    return QStandardPaths::writableLocation(QStandardPaths::HomeLocation).toStdString();
}

FilePath PlatformCompatibility::GetLongFileName(const FilePath& shortFileName)
{
    // Use Qt's canonicalFilePath to resolve short paths, symlinks, and redundant elements
    // This works cross-platform and handles Windows short paths (e.g., PROGRA~1 -> Program Files)

    // Convert wxString (FilePath) to QString
    QString qPath = QString::fromStdWString(shortFileName.ToStdWstring());
    QString canonical = QFileInfo(qPath).canonicalFilePath();

    // Convert back to wxString (FilePath)
    return canonical.isEmpty() ? shortFileName : FilePath(canonical.toStdWString());
}
