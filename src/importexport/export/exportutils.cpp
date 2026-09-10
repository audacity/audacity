/*
 * Audacity: A Digital Audio Editor
 */

#include "exportutils.h"

#include <algorithm>

#include "framework/global/stringutils.h"

muse::Val au::importexport::utils::matrixToVal(const std::vector<std::vector<bool> >& matrix)
{
    muse::ValList rows;
    rows.reserve(matrix.size());

    for (const auto& row : matrix) {
        muse::ValList cols;
        cols.reserve(row.size());
        for (bool val : row) {
            cols.emplace_back(muse::Val(val));
        }
        rows.emplace_back(muse::Val(cols));
    }

    return muse::Val(rows);
}

std::vector<std::vector<bool> > au::importexport::utils::valToMatrix(const muse::Val& val)
{
    std::vector<std::vector<bool> > matrix;

    if (val.type() != muse::Val::Type::List) {
        return matrix;
    }

    const muse::ValList rows = val.toList();
    matrix.resize(rows.size());

    for (size_t rIdx = 0; rIdx < rows.size(); ++rIdx) {
        if (rows[rIdx].type() != muse::Val::Type::List) {
            continue;
        }

        const muse::ValList cols = rows[rIdx].toList();
        matrix[rIdx].resize(cols.size());

        for (size_t cIdx = 0; cIdx < cols.size(); ++cIdx) {
            matrix[rIdx][cIdx] = cols[cIdx].toBool();
        }
    }

    return matrix;
}

std::string au::importexport::utils::separateFileName(const std::string& prefix, std::optional<int> number, const std::string& name)
{
    std::string result;
    const auto append = [&result](const std::string& part) {
        if (part.empty()) {
            return;
        }
        if (!result.empty()) {
            result += ".";
        }
        result += part;
    };

    append(prefix);
    if (number.has_value()) {
        const std::string digits = std::to_string(number.value());
        append(digits.size() < 2 ? "0" + digits : digits);
    }
    append(name);

    return result;
}

std::string au::importexport::utils::makeFileNameUnique(const std::string& name, std::vector<std::string>& otherNames)
{
    const auto isUsed = [&otherNames](const std::string& candidate) {
        const std::string lowered = muse::strings::toLower(candidate);
        return std::any_of(otherNames.begin(), otherNames.end(), [&lowered](const std::string& other) {
            return muse::strings::toLower(other) == lowered;
        });
    };

    std::string result = name;
    for (int i = 2; isUsed(result); ++i) {
        result = name + "-" + std::to_string(i);
    }

    otherNames.push_back(result);
    return result;
}

std::vector<au::importexport::utils::TimeRange> au::importexport::utils::labelExportRanges(const std::vector<TimeRange>& labels,
                                                                                           double projectEndTime)
{
    std::vector<TimeRange> ranges;
    ranges.reserve(labels.size());

    for (size_t i = 0; i < labels.size(); ++i) {
        TimeRange range;
        range.start = labels[i].start;
        if (labels[i].end > labels[i].start) {
            range.end = labels[i].end;
        } else if (i + 1 < labels.size()) {
            range.end = labels[i + 1].start;
        } else {
            range.end = projectEndTime;
        }
        ranges.push_back(range);
    }

    return ranges;
}
