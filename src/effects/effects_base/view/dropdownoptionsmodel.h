/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "uicomponents/qml/Muse/UiComponents/menuitem.h"

#include <QObject>

namespace au::effects {
class DropdownOptionsModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QString label READ label WRITE setLabel NOTIFY labelChanged)
    Q_PROPERTY(muse::uicomponents::MenuItemList options READ options WRITE setOptions NOTIFY optionsChanged)
    Q_PROPERTY(QString currentTitle READ currentTitle NOTIFY currentChanged)
    Q_PROPERTY(int selectedIndex READ selectedIndex NOTIFY currentChanged)

public:
    explicit DropdownOptionsModel(QObject* parent = nullptr);

    QString label() const { return m_label; }
    void setLabel(const QString& label);

    void setOptions(const muse::uicomponents::MenuItemList& options);

    muse::uicomponents::MenuItemList options() const;
    QString currentTitle() const;
    int selectedIndex() const { return m_selectedIndex; }

    Q_INVOKABLE void select(const QString& optionId);

signals:
    void labelChanged();
    void optionsChanged();
    void currentChanged();

private:
    QString m_label;
    muse::uicomponents::MenuItemList m_options;
    int m_selectedIndex = 0;
};
}
