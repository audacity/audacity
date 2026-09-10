/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <optional>
#include <string>
#include <vector>

#include "types/val.h"

namespace au::importexport::utils {
muse::Val matrixToVal(const std::vector<std::vector<bool> >& matrix);
std::vector<std::vector<bool> > valToMatrix(const muse::Val& val);

std::string separateFileName(const std::string& prefix, std::optional<int> number, const std::string& name);
std::string makeFileNameUnique(const std::string& name, std::vector<std::string>& otherNames);

struct TimeRange {
    double start = 0.0;
    double end = 0.0;
};

std::vector<TimeRange> labelExportRanges(const std::vector<TimeRange>& labels, double projectEndTime);
}
