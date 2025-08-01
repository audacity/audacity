/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "uicomponents/view/toolbaritem.h"

namespace au::playback {
class PlaybackToolBarBPMItem : public muse::uicomponents::ToolBarItem
{
    Q_OBJECT

    Q_PROPERTY(double currentValue READ currentValue WRITE setCurrentValue NOTIFY currentValueChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;

public:
    explicit PlaybackToolBarBPMItem(const muse::ui::UiAction& action, muse::uicomponents::ToolBarItemType::Type type,
                                    QObject* parent = nullptr);

    double currentValue() const;
    void setCurrentValue(double value);

signals:
    void currentValueChanged();

private:
    void onProjectChanged();

    void updateValues();

    context::IPlaybackStatePtr playbackState() const;

    double m_currentValue = 0.0;
};
}
