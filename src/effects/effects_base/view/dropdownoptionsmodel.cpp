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

void DropdownOptionsModel::setOptions(const QList<DropdownOption>& options)
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
    QVariantList result;
    for (int i = 0; i < m_options.size(); ++i) {
        QVariantMap item;
        item["id"] = m_options[i].id;
        item["title"] = m_options[i].title;
        item["enabled"] = true;
        item["selectable"] = true;
        item["selected"] = (i == m_selectedIndex);
        result.append(item);
    }
    return result;
}

QString DropdownOptionsModel::currentTitle() const
{
    if (m_selectedIndex >= 0 && m_selectedIndex < m_options.size()) {
        return m_options[m_selectedIndex].title;
    }
    return {};
}

void DropdownOptionsModel::select(const QString& optionId)
{
    for (int i = 0; i < m_options.size(); ++i) {
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
