/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

namespace au::effects {
class AbstractEffectModel : public QObject
{
    Q_OBJECT

public:
    AbstractEffectModel(QObject* parent = nullptr);
};
}
