/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QTimer>

#include "au3audio/isystemaudiodeviceslistener.h"

namespace au::au3audio {
class MacosSystemAudioDevicesListener : public QObject, public ISystemAudioDevicesListener
{
public:
    MacosSystemAudioDevicesListener();
    ~MacosSystemAudioDevicesListener() override;

    void startListening() override;
    void stopListening() override;

    muse::async::Notification systemDevicesChanged() const override;

    //! NOTE Called from a CoreAudio thread; hops to this object's thread
    void onSystemDevicesChanged();

private:
    QTimer m_debounceTimer;
    bool m_listening = false;

    muse::async::Notification m_systemDevicesChanged;
};
}
