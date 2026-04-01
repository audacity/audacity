/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "uicomponents/qml/Muse/UiComponents/menuitem.h"

#include <QObject>

namespace au::effects {
struct DropdownOption {
    QString id;
    QString title;
};

class DropdownOptionsModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QString label READ label CONSTANT)
    Q_PROPERTY(muse::uicomponents::MenuItemList options READ options NOTIFY optionsChanged)
    Q_PROPERTY(QString currentTitle READ currentTitle NOTIFY currentChanged)

public:
    explicit DropdownOptionsModel(const QString& label, QObject* parent = nullptr);

    QString label() const { return m_label; }

    void setOptions(const std::vector<DropdownOption>& options);

    muse::uicomponents::MenuItemList options() const;
    QString currentTitle() const;

    Q_INVOKABLE void select(const QString& optionId);

signals:
    void optionsChanged();
    void currentChanged();

private:
    const QString m_label;
    std::vector<DropdownOption> m_options;
    int m_selectedIndex = 0;
};
}
