/*
 * Audacity: A Digital Audio Editor
 */
#include "mousehelper.h"

namespace au::projectscene {
void MouseHelper::callUngrabMouseOnItem(QQuickItem* item)
{
    if (item) {
        item->ungrabMouse();
    }
}
}
