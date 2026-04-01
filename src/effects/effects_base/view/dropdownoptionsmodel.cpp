/*
 * Audacity: A Digital Audio Editor
 */
#include "dropdownoptionsmodel.h"

#include "framework/global/log.h"

namespace au::effects {
DropdownOptionsModel::DropdownOptionsModel(const QString& label, QObject* parent)
    : QObject(parent), m_label(label)
{
}

void DropdownOptionsModel::setOptions(const std::vector<DropdownOption>& options)
{
    if (m_options == options) {
        return;
    }
    m_options = options;
    m_selectedIndex = 0;
    emit optionsChanged();
    emit currentChanged();
}

QVariantList DropdownOptionsModel::options() const
{
    for (auto i = 0; i < static_cast<int>(m_options.size()); ++i) {
        result.append(item);
    }
    return result;
}

QString DropdownOptionsModel::currentTitle() const
{
    if (m_selectedIndex >= 0 && m_selectedIndex < static_cast<int>(m_options.size())) {
        return m_options[m_selectedIndex].title;
    }
    return {};
}

void DropdownOptionsModel::select(const QString& optionId)
{
    for (auto i = 0; i < static_cast<int>(m_options.size()); ++i) {
        if (m_options[i].id == optionId) {
            if (i != m_selectedIndex) {
                m_selectedIndex = i;
                emit optionsChanged();
                emit currentChanged();
            }
            return;
        }
    }
    LOGE() << "invalid option: " << optionId;
}
}
