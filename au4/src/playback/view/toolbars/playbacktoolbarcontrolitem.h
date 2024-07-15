/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QString>

#include "modularity/ioc.h"
#include "playback/iplayback.h"

#include "uicomponents/view/toolbaritem.h"

namespace au::playback {
class PlaybackToolBarControlItem : public muse::uicomponents::ToolBarItem
{
    Q_OBJECT

    Q_PROPERTY(QColor iconColor READ iconColor WRITE setIconColor NOTIFY iconColorChanged)
    Q_PROPERTY(QColor backgroundColor READ backgroundColor WRITE setBackgroundColor NOTIFY backgroundColorChanged)

    muse::Inject<IPlayback> playback;

public:
    explicit PlaybackToolBarControlItem(const muse::ui::UiAction& action, muse::uicomponents::ToolBarItemType::Type type,
                                        QObject* parent = nullptr);

    QColor iconColor() const;
    void setIconColor(const QColor& color);

    QColor backgroundColor() const;
    void setBackgroundColor(const QColor& color);

signals:
    void iconColorChanged();

    void backgroundColorChanged();

private:
    QColor m_iconColor;
    QColor m_backgroundColor;
};
}
