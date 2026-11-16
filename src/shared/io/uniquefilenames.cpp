/*
 * Audacity: A Digital Audio Editor
 */
#include "uniquefilenames.h"

#include <QString>

std::string au::shared::UniqueFileNames::registerName(const std::string& name)
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
