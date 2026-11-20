/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <string_view>

#include <QString>
#include <QVariantMap>

#include "framework/global/io/path.h"
#include "framework/global/types/val.h"
#include "project/iaudacityproject.h"

namespace au::importexport {
QString stepDefaultFolder(const muse::io::path_t& exportDir);
QString stepOutputPreview(const muse::io::path_t& exportDir, const project::IAudacityProjectPtr& project, const std::string& folder,
                          const std::string& fileName, std::string_view extension);
QString stepFileNameWarning(const std::string& fileName);

muse::Val stepDataFromMap(const QVariantMap& map);
QVariantMap stepDataToMap(const muse::Val& val);
}
