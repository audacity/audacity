/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#ifndef AU_APPSHELL_PLAYBACKPREFERENCESMODEL_H
#define AU_APPSHELL_PLAYBACKPREFERENCESMODEL_H

#include <QObject>

#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "audio/iaudioconfiguration.h"
// #include "midi/imidiconfiguration.h"
// #include "midi/imidioutport.h"
// #include "midi/imidiinport.h"
#include "playback/iaudiodevicesprovider.h"
#include "playback/iplaybackconfiguration.h"

namespace au::appshell {
class PlaybackPreferencesModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    INJECT(muse::audio::IAudioConfiguration, audioConfiguration)
    // INJECT(midi::IMidiConfiguration, midiConfiguration)
    // INJECT(midi::IMidiOutPort, midiOutPort)
    // INJECT(midi::IMidiInPort, midiInPort)
    INJECT(playback::IPlaybackConfiguration, playbackConfiguration)
    muse::Inject<playback::IAudioDevicesProvider> audioDevicesProvider;

    Q_PROPERTY(int currentAudioApiIndex READ currentAudioApiIndex WRITE setCurrentAudioApiIndex NOTIFY currentAudioApiIndexChanged)

    // Q_PROPERTY(QVariantList midiInputDevices READ midiInputDevices NOTIFY midiInputDevicesChanged)
    // Q_PROPERTY(QString midiInputDeviceId READ midiInputDeviceId NOTIFY midiInputDeviceIdChanged)

    // Q_PROPERTY(QVariantList midiOutputDevices READ midiOutputDevices NOTIFY midiOutputDevicesChanged)
    // Q_PROPERTY(QString midiOutputDeviceId READ midiOutputDeviceId NOTIFY midiOutputDeviceIdChanged)

    // Q_PROPERTY(bool isMIDI20OutputSupported READ isMIDI20OutputSupported CONSTANT)
    // Q_PROPERTY(bool useMIDI20Output READ useMIDI20Output WRITE setUseMIDI20Output NOTIFY useMIDI20OutputChanged)

    // Q_PROPERTY(bool muteHiddenInstruments READ muteHiddenInstruments WRITE setMuteHiddenInstruments NOTIFY muteHiddenInstrumentsChanged)

public:
    explicit PlaybackPreferencesModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    int currentAudioApiIndex() const;

    // QString midiInputDeviceId() const;
    // Q_INVOKABLE void inputDeviceSelected(const QString& deviceId);

    // QString midiOutputDeviceId() const;
    // Q_INVOKABLE void outputDeviceSelected(const QString& deviceId);

    Q_INVOKABLE QStringList audioApiList() const;

    // Q_INVOKABLE void restartAudioAndMidiDevices();

    // QVariantList midiInputDevices() const;
    // QVariantList midiOutputDevices() const;

    // bool isMIDI20OutputSupported() const;
    // bool useMIDI20Output() const;

    // bool muteHiddenInstruments() const;

public slots:
    void setCurrentAudioApiIndex(int index);

    // void setUseMIDI20Output(bool use);

    // void setMuteHiddenInstruments(bool mute);

signals:
    void currentAudioApiIndexChanged(int index);
    // void midiInputDeviceIdChanged();
    // void midiOutputDeviceIdChanged();

    // void midiInputDevicesChanged();
    // void midiOutputDevicesChanged();

    // void useMIDI20OutputChanged();

    // void muteHiddenInstrumentsChanged(bool mute);

// private:
    // midi::MidiDeviceID midiInputDeviceId(int index) const;
    // midi::MidiDeviceID midiOutputDeviceId(int index) const;

    // void showMidiError(const midi::MidiDeviceID& deviceId, const std::string& text) const;
};
}

#endif // AU_APPSHELL_PLAYBACKPREFERENCESMODEL_H
