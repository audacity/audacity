/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

namespace au::effects {
class AbstractEffectModel : public QObject
{
public:
    AbstractEffectModel(QObject* parent = nullptr);
};
}
