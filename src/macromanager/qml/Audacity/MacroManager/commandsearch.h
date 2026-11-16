/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>

#include <QString>

namespace au::macromanager {
struct CommandSearchResult {
    double score = 0.0;
    QString highlightedTitle;
};

std::optional<CommandSearchResult> searchCommand(const QString& query, const QString& title, const QString& subtitle);
}
