/*
 * Audacity: A Digital Audio Editor
 */
#include "dropdownoptionsmodel.h"

#include "framework/global/log.h"

namespace au::effects {
DropdownOptionsModel::DropdownOptionsModel(QObject* parent)
    : QObject(parent)
{
}

void DropdownOptionsModel::setLabel(const QString& label)
{
    if (m_label != label) {
        m_label = label;
        emit labelChanged();
    }
}

void DropdownOptionsModel::setOptions(const muse::uicomponents::MenuItemList& options)
{
    if (m_options == options) {
        return;
    }
    m_options = options;
    emit optionsChanged();
}

muse::uicomponents::MenuItemList DropdownOptionsModel::options() const
{
    return m_options;
}

QString DropdownOptionsModel::currentTitle() const
{
    if (m_selectedIndex >= 0 && m_selectedIndex < static_cast<int>(m_options.size())) {
        return m_options[m_selectedIndex]->translatedTitle();
    }
    return {};
}

void DropdownOptionsModel::select(const QString& optionId)
{
    for (auto i = 0; i < static_cast<int>(m_options.size()); ++i) {
        if (m_options[i]->id() == optionId) {
            if (i != m_selectedIndex) {
                m_selectedIndex = i;
                emit currentChanged();
            }
            return;
        }
    }
    LOGE() << "invalid option: " << optionId;
}
}
