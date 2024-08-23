/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

namespace au::effects {
class AbstractEffectModel : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QString instanceId READ instanceId WRITE setInstanceId NOTIFY instanceIdChanged FINAL)

public:
    AbstractEffectModel(QObject* parent = nullptr);

    QString instanceId() const;
    void setInstanceId(const QString& newInstanceId);

signals:
    void instanceIdChanged();

private:
    QString m_instanceId;
};
}
