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

#include "noteinputpreferencesmodel.h"

#include "log.h"

using namespace au::appshell;

NoteInputPreferencesModel::NoteInputPreferencesModel(QObject* parent)
    : QObject(parent)
{
}

bool NoteInputPreferencesModel::advanceToNextNoteOnKeyRelease() const
{
    return shortcutsConfiguration()->advanceToNextNoteOnKeyRelease();
}

bool NoteInputPreferencesModel::colorNotesOutsideOfUsablePitchRange() const
{
    return notationConfiguration()->colorNotesOutsideOfUsablePitchRange();
}

bool NoteInputPreferencesModel::warnGuitarBends() const
{
    return notationConfiguration()->warnGuitarBends();
}

int NoteInputPreferencesModel::delayBetweenNotesInRealTimeModeMilliseconds() const
{
    return notationConfiguration()->delayBetweenNotesInRealTimeModeMilliseconds();
}

bool NoteInputPreferencesModel::playNotesWhenEditing() const
{
    return playbackConfiguration()->playNotesWhenEditing();
}

int NoteInputPreferencesModel::notePlayDurationMilliseconds() const
{
    return notationConfiguration()->notePlayDurationMilliseconds();
}

bool NoteInputPreferencesModel::playChordWhenEditing() const
{
    return playbackConfiguration()->playChordWhenEditing();
}

bool NoteInputPreferencesModel::playChordSymbolWhenEditing() const
{
    return playbackConfiguration()->playHarmonyWhenEditing();
}

void NoteInputPreferencesModel::setAdvanceToNextNoteOnKeyRelease(bool value)
{
    if (value == advanceToNextNoteOnKeyRelease()) {
        return;
    }

    shortcutsConfiguration()->setAdvanceToNextNoteOnKeyRelease(value);
    emit advanceToNextNoteOnKeyReleaseChanged(value);
}

void NoteInputPreferencesModel::setColorNotesOutsideOfUsablePitchRange(bool value)
{
    if (value == colorNotesOutsideOfUsablePitchRange()) {
        return;
    }

    notationConfiguration()->setColorNotesOutsideOfUsablePitchRange(value);
    emit colorNotesOutsideOfUsablePitchRangeChanged(value);
}

void NoteInputPreferencesModel::setWarnGuitarBends(bool value)
{
    if (value == warnGuitarBends()) {
        return;
    }

    notationConfiguration()->setWarnGuitarBends(value);
    emit warnGuitarBendsChanged(value);
}

void NoteInputPreferencesModel::setDelayBetweenNotesInRealTimeModeMilliseconds(int delay)
{
    if (delay == delayBetweenNotesInRealTimeModeMilliseconds()) {
        return;
    }

    notationConfiguration()->setDelayBetweenNotesInRealTimeModeMilliseconds(delay);
    emit delayBetweenNotesInRealTimeModeMillisecondsChanged(delay);
}

void NoteInputPreferencesModel::setPlayNotesWhenEditing(bool value)
{
    if (value == playNotesWhenEditing()) {
        return;
    }

    playbackConfiguration()->setPlayNotesWhenEditing(value);
    emit playNotesWhenEditingChanged(value);
}

void NoteInputPreferencesModel::setNotePlayDurationMilliseconds(int duration)
{
    if (duration == notePlayDurationMilliseconds()) {
        return;
    }

    notationConfiguration()->setNotePlayDurationMilliseconds(duration);
    emit notePlayDurationMillisecondsChanged(duration);
}

void NoteInputPreferencesModel::setPlayChordWhenEditing(bool value)
{
    if (value == playChordWhenEditing()) {
        return;
    }

    playbackConfiguration()->setPlayChordWhenEditing(value);
    emit playChordWhenEditingChanged(value);
}

void NoteInputPreferencesModel::setPlayChordSymbolWhenEditing(bool value)
{
    if (value == playChordSymbolWhenEditing()) {
        return;
    }

    playbackConfiguration()->setPlayHarmonyWhenEditing(value);
    emit playChordSymbolWhenEditingChanged(value);
}
