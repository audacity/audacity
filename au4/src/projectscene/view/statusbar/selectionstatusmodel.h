/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

namespace au::projectscene {
class SelectionStatusModel : public QObject
{
    Q_OBJECT

public:
    SelectionStatusModel();
};
}
