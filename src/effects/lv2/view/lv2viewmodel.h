/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include <QQuickItem>

namespace au::effects {
class Lv2ViewModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QQuickItem * contentItem READ contentItem NOTIFY contentItemChanged FINAL)

public:
    Lv2ViewModel(QObject* parent = nullptr);
    ~Lv2ViewModel() override;

    QQuickItem* contentItem() const;

    Q_INVOKABLE void load(const QString& instanceId, QObject* itemParent);

signals:
    void contentItemChanged();
    void closeRequested();

private:
    QQuickItem* m_contentItem = nullptr;
};
}
