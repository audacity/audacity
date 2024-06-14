/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QString>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "actions/iactionsdispatcher.h"
#include "playback/iplayback.h"

#include "uicomponents/view/toolbaritem.h"

namespace au::playback {
class PlaybackToolBarTimeItem : public muse::uicomponents::ToolBarItem
{
    Q_OBJECT

    Q_PROPERTY(int currentFormat READ currentFormat WRITE setCurrentFormat NOTIFY currentFormatChanged FINAL)

    Q_PROPERTY(double currentValue READ currentValue WRITE setCurrentValue NOTIFY currentValueChanged FINAL)

    Q_PROPERTY(double sampleRate READ sampleRate NOTIFY sampleRateChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<playback::IPlayback> playback;

public:
    explicit PlaybackToolBarTimeItem(const muse::ui::UiAction& action, muse::uicomponents::ToolBarItemType::Type type,
                                     QObject* parent = nullptr);

    int currentFormat() const;
    void setCurrentFormat(int format);

    double currentValue() const;
    void setCurrentValue(double value);

    double sampleRate() const;

signals:
    void currentFormatChanged();
    void currentValueChanged();
    void sampleRateChanged();

private:
    context::IPlaybackStatePtr playbackState() const;

    int m_currentFormat = 0;
};
}
