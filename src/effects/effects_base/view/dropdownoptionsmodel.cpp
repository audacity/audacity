/*
 * Audacity: A Digital Audio Editor
 */
#include "dropdownoptionsmodel.h"

#include "framework/global/log.h"
#include "types/translatablestring.h"

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

muse::uicomponents::MenuItemList DropdownOptionsModel::options() const
{
    muse::uicomponents::MenuItemList result;
    for (auto i = 0; i < static_cast<int>(m_options.size()); ++i) {
        auto item = new muse::uicomponents::MenuItem();
        item->setId(m_options[i].id);
        item->setTitle(muse::TranslatableString::untranslatable({ m_options[i].title }));
        item->setSelectable(true);
        item->setSelected((i == m_selectedIndex));
        auto state = item->state();
        state.enabled = true;
        item->setState(state);
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
