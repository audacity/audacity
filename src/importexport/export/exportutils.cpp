/*
 * Audacity: A Digital Audio Editor
 */

#include "exportutils.h"

#include <cctype>

#include <QString>

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

std::string au::importexport::utils::formatFileName(const std::string& prefix, std::optional<int> number, const std::string& name)
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

namespace {
bool isReservedDeviceName(const std::string& stem)
{
    static const std::set<std::string> reserved = {
        "CON", "PRN", "AUX", "NUL",
        "COM0", "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9", "COM\u00b9", "COM\u00b2", "COM\u00b3",
        "LPT0", "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9", "LPT\u00b9", "LPT\u00b2", "LPT\u00b3",
    };

    std::string upper = stem;
    for (char& ch : upper) {
        ch = static_cast<char>(std::toupper(static_cast<unsigned char>(ch)));
    }

    return reserved.count(upper) > 0;
}
}

std::string au::importexport::utils::sanitizeFileName(const std::string& name)
{
    static const std::string invalidChars = "\\/:*?\"<>|~";

    std::string result = name;
    for (char& ch : result) {
        const unsigned char code = static_cast<unsigned char>(ch);
        if (code < 0x20 || code == 0x7F || invalidChars.find(ch) != std::string::npos) {
            ch = '_';
        }
    }

    const size_t stemEnd = result.find('.');
    if (isReservedDeviceName(result.substr(0, stemEnd))) {
        result.insert(stemEnd == std::string::npos ? result.size() : stemEnd, "_");
    }

    return result;
}

std::string au::importexport::utils::UniqueFileNames::registerName(const std::string& name)
{
    const auto fold = [](const std::string& value) {
        return QString::fromStdString(value).normalized(QString::NormalizationForm_C).toCaseFolded().toStdString();
    };

    std::string result = name;
    std::string folded = fold(result);
    for (int i = 2; m_foldedNames.count(folded) > 0; ++i) {
        result = name + "-" + std::to_string(i);
        folded = fold(result);
    }

    m_foldedNames.insert(folded);
    return result;
}

std::string au::importexport::utils::makeFileName(const std::string& prefix, std::optional<int> number, const std::string& name,
                                                  UniqueFileNames& usedNames)
{
    return usedNames.registerName(sanitizeFileName(formatFileName(prefix, number, name)));
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
