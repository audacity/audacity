/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <optional>
#include <string>
#include <vector>

#include "types/val.h"

#include "shared/io/uniquefilenames.h"

namespace au::importexport::utils {
muse::Val matrixToVal(const std::vector<std::vector<bool> >& matrix);
std::vector<std::vector<bool> > valToMatrix(const muse::Val& val);

std::string formatFileName(const std::string& prefix, std::optional<int> number, const std::string& name);
std::string sanitizeFileName(const std::string& name);
std::string makeFileName(const std::string& prefix, std::optional<int> number, const std::string& name, shared::UniqueFileNames& usedNames);

struct TimeRange {
    double start = 0.0;
    double end = 0.0;
};

std::vector<TimeRange> labelExportRanges(const std::vector<TimeRange>& labels, double projectEndTime);
}
