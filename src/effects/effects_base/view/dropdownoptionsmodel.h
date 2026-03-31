/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

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
    Q_PROPERTY(QVariantList options READ options NOTIFY optionsChanged)
    Q_PROPERTY(QString currentTitle READ currentTitle NOTIFY currentChanged)

public:
    explicit DropdownOptionsModel(const QString& label, QObject* parent = nullptr);

    QString label() const { return m_label; }

    void setOptions(const QList<DropdownOption>& options);

    QVariantList options() const;
    QString currentTitle() const;

    Q_INVOKABLE void select(const QString& optionId);

signals:
    void optionsChanged();
    void currentChanged();

private:
    const QString m_label;
    QList<DropdownOption> m_options;
    int m_selectedIndex = 0;
};
}
