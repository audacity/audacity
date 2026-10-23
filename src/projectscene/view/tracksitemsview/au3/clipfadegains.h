/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QList>
#include <QObject>

#include "types/projectscenetypes.h"

namespace au::projectscene {
//! Gives QML the fade gains of `ClipFades`, e.g. to draw the fades
class ClipFadeGains : public QObject
{
    Q_OBJECT

public:
    explicit ClipFadeGains(QObject* parent = nullptr);

    //! The gains of a fade of type `type` at `positions`, from 0, at the silent end of the fade, to 1
    Q_INVOKABLE QList<qreal> gainsAt(FadeTypes::Type type, const QList<qreal>& positions) const;
};
}
