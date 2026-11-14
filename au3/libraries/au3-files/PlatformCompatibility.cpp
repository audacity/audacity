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

#ifdef Q_OS_WIN
#include <windows.h>
#include <Lmcons.h>
#else
#include <unistd.h>
#include <pwd.h>
#endif

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

std::string PlatformCompatibility::GetExecutableDir()
{
    return QCoreApplication::applicationDirPath().toStdString();
}

std::string PlatformCompatibility::GetCurrentDir()
{
    return QDir::currentPath().toStdString();
}

std::string PlatformCompatibility::GetUserId()
{
#ifdef Q_OS_WIN
    wchar_t username[UNLEN + 1];
    DWORD size = UNLEN + 1;
    if (GetUserNameW(username, &size)) {
        return QString::fromWCharArray(username).toStdString();
    }
    return qEnvironmentVariable("USERNAME").toStdString();
#else
    if (struct passwd* pw = getpwuid(getuid())) {
        return std::string(pw->pw_name);
    }
    return std::to_string(getuid());
#endif
}

std::string PlatformCompatibility::GetParentDir(const std::string& path)
{
    return QFileInfo(QString::fromStdString(path)).path().toStdString();
}

std::string PlatformCompatibility::GetEnvironmentVar(const char* name)
{
    return qEnvironmentVariable(name).toStdString();
}

bool PlatformCompatibility::DirectoryExists(const std::string& path)
{
    return QDir(QString::fromStdString(path)).exists();
}

bool PlatformCompatibility::MakePath(const std::string& path)
{
    return QDir().mkpath(QString::fromStdString(path));
}

std::string PlatformCompatibility::NormalizePath(const std::string& path)
{
    if (path.empty()) {
        return path;
    }
    QString normalized = QDir(QString::fromStdString(path)).absolutePath();
    return normalized.isEmpty() ? path : normalized.toStdString();
}

std::vector<std::string> PlatformCompatibility::SplitSearchPath(const std::string& pathString)
{
    std::vector<std::string> result;
    if (pathString.empty()) {
        return result;
    }

#ifdef Q_OS_WIN
    const QChar sep = ';';
#else
    const QChar sep = ':';
#endif

    QString qstr = QString::fromStdString(pathString);
    for (const QString& p : qstr.split(sep, Qt::SkipEmptyParts)) {
        std::string trimmed = p.trimmed().toStdString();
        if (!trimmed.empty()) {
            result.push_back(trimmed);
        }
    }
    return result;
}
