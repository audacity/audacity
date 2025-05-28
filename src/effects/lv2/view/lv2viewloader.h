/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include <QQuickItem>

namespace au::effects {
class Lv2ViewLoader : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QQuickItem * contentItem READ contentItem NOTIFY contentItemChanged FINAL)

public:
    Lv2ViewLoader(QObject* parent = nullptr);
    ~Lv2ViewLoader() override;

    QQuickItem* contentItem() const;

    Q_INVOKABLE void load(const QString& instanceId, QObject* itemParent);

signals:
    void contentItemChanged();
    void closeRequested();

private:
    QQuickItem* m_contentItem = nullptr;
};
}
