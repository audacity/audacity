#pragma once

#include <QObject>
#include "processing/processingtypes.h"

namespace au::projectscene {
class ClipKey
{
    Q_GADGET

public:
    ClipKey();

    processing::ClipKey key;
};
}
