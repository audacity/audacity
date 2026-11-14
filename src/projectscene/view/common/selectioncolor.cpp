/*
* Audacity: A Digital Audio Editor
*/
#include "selectioncolor.h"

#include <QtGlobal>

QColor au::projectscene::timeSelectionColor(const QColor& originalColor)
{
    const int r = originalColor.red();
    const int g = originalColor.green();
    const int b = originalColor.blue();

    const int deltaRed = (r < 240) ? 51 : (255 - r);
    const int deltaGreen = (g < 240) ? 69 : (255 - g);
    const int deltaBlue = 77;

    return QColor(qBound(0, r + deltaRed, 255), qBound(0, g + deltaGreen, 255), qBound(0, b + deltaBlue, 255));
}
