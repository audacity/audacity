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
#ifndef AU_APPSHELL_NOTEINPUTPREFERENCESMODEL_H
#define AU_APPSHELL_NOTEINPUTPREFERENCESMODEL_H

#include <QObject>

#include "modularity/ioc.h"
#include "shortcuts/ishortcutsconfiguration.h"
#include "notation/inotationconfiguration.h"
#include "playback/iplaybackconfiguration.h"

namespace au::appshell {
class NoteInputPreferencesModel : public QObject
{
    Q_OBJECT

    INJECT(shortcuts::IShortcutsConfiguration, shortcutsConfiguration)
    INJECT(notation::INotationConfiguration, notationConfiguration)
    INJECT(playback::IPlaybackConfiguration, playbackConfiguration)

    Q_PROPERTY(
        bool advanceToNextNoteOnKeyRelease READ advanceToNextNoteOnKeyRelease WRITE setAdvanceToNextNoteOnKeyRelease NOTIFY advanceToNextNoteOnKeyReleaseChanged)
    Q_PROPERTY(
        bool colorNotesOutsideOfUsablePitchRange READ colorNotesOutsideOfUsablePitchRange WRITE setColorNotesOutsideOfUsablePitchRange NOTIFY colorNotesOutsideOfUsablePitchRangeChanged)
    Q_PROPERTY(
        bool warnGuitarBends READ warnGuitarBends WRITE setWarnGuitarBends NOTIFY warnGuitarBendsChanged)
    Q_PROPERTY(
        int delayBetweenNotesInRealTimeModeMilliseconds READ delayBetweenNotesInRealTimeModeMilliseconds WRITE setDelayBetweenNotesInRealTimeModeMilliseconds NOTIFY delayBetweenNotesInRealTimeModeMillisecondsChanged)

    Q_PROPERTY(bool playNotesWhenEditing READ playNotesWhenEditing WRITE setPlayNotesWhenEditing NOTIFY playNotesWhenEditingChanged)
    Q_PROPERTY(
        int notePlayDurationMilliseconds READ notePlayDurationMilliseconds WRITE setNotePlayDurationMilliseconds NOTIFY notePlayDurationMillisecondsChanged)
    Q_PROPERTY(bool playChordWhenEditing READ playChordWhenEditing WRITE setPlayChordWhenEditing NOTIFY playChordWhenEditingChanged)
    Q_PROPERTY(
        bool playChordSymbolWhenEditing READ playChordSymbolWhenEditing WRITE setPlayChordSymbolWhenEditing NOTIFY playChordSymbolWhenEditingChanged)

public:
    explicit NoteInputPreferencesModel(QObject* parent = nullptr);

    bool advanceToNextNoteOnKeyRelease() const;
    bool colorNotesOutsideOfUsablePitchRange() const;
    bool warnGuitarBends() const;
    int delayBetweenNotesInRealTimeModeMilliseconds() const;

    bool playNotesWhenEditing() const;
    int notePlayDurationMilliseconds() const;
    bool playChordWhenEditing() const;
    bool playChordSymbolWhenEditing() const;

public slots:
    void setAdvanceToNextNoteOnKeyRelease(bool value);
    void setColorNotesOutsideOfUsablePitchRange(bool value);
    void setWarnGuitarBends(bool value);
    void setDelayBetweenNotesInRealTimeModeMilliseconds(int delay);
    void setPlayNotesWhenEditing(bool value);
    void setNotePlayDurationMilliseconds(int duration);
    void setPlayChordWhenEditing(bool value);
    void setPlayChordSymbolWhenEditing(bool value);

signals:
    void advanceToNextNoteOnKeyReleaseChanged(bool value);
    void colorNotesOutsideOfUsablePitchRangeChanged(bool value);
    void warnGuitarBendsChanged(bool value);
    void delayBetweenNotesInRealTimeModeMillisecondsChanged(int delay);
    void playNotesWhenEditingChanged(bool value);
    void notePlayDurationMillisecondsChanged(int duration);
    void playChordWhenEditingChanged(bool value);
    void playChordSymbolWhenEditingChanged(bool value);
};
}

#endif // AU_APPSHELL_NOTEINPUTPREFERENCESMODEL_H
