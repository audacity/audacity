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
#include "macosshortcutsinstancemodel.h"

#import <Carbon/Carbon.h>

#include <QApplication>
#include <QKeyEvent>

#include <private/qguiapplication_p.h>
#include <qpa/qplatformintegration.h>

#include "log.h"

using namespace mu::shortcuts;

static UCKeyboardLayout* keyboardLayout()
{
    static TISInputSourceRef (* TISCopyCurrentKeyboardLayoutInputSource)(void);
    static void*(* TISGetInputSourceProperty)(TISInputSourceRef inputSource, CFStringRef propertyKey);

    CFBundleRef bundle = CFBundleGetBundleWithIdentifier(CFSTR("com.apple.Carbon"));

    if (bundle) {
        *(void**)& TISGetInputSourceProperty = CFBundleGetFunctionPointerForName(bundle, CFSTR("TISGetInputSourceProperty"));
        *(void**)& TISCopyCurrentKeyboardLayoutInputSource
            = CFBundleGetFunctionPointerForName(bundle, CFSTR("TISCopyCurrentKeyboardLayoutInputSource"));
    }

    if (!TISCopyCurrentKeyboardLayoutInputSource || !TISGetInputSourceProperty) {
        LOGE() << "Error getting functions from Carbon framework";
        return 0;
    }

    TISInputSourceRef currentKeyboard = TISCopyCurrentKeyboardLayoutInputSource();
    CFDataRef uchr = (CFDataRef)TISGetInputSourceProperty(currentKeyboard, CFSTR("TISPropertyUnicodeKeyLayoutData"));

    return (UCKeyboardLayout*)CFDataGetBytePtr(uchr);
}

static quint32 nativeKeycode(UCKeyboardLayout* keyboard, Qt::Key keyCode)
{
    switch (keyCode) {
    case Qt::Key_Return:
        return kVK_Return;
    case Qt::Key_Enter:
        return kVK_ANSI_KeypadEnter;
    case Qt::Key_Tab:
        return kVK_Tab;
    case Qt::Key_Space:
        return kVK_Space;
    case Qt::Key_Backspace:
        return kVK_Delete;
    case Qt::Key_Escape:
        return kVK_Escape;
    case Qt::Key_CapsLock:
        return kVK_CapsLock;
    case Qt::Key_Option:
        return kVK_Option;
    case Qt::Key_F17:
        return kVK_F17;
    case Qt::Key_VolumeUp:
        return kVK_VolumeUp;
    case Qt::Key_VolumeDown:
        return kVK_VolumeDown;
    case Qt::Key_F18:
        return kVK_F18;
    case Qt::Key_F19:
        return kVK_F19;
    case Qt::Key_F20:
        return kVK_F20;
    case Qt::Key_F5:
        return kVK_F5;
    case Qt::Key_F6:
        return kVK_F6;
    case Qt::Key_F7:
        return kVK_F7;
    case Qt::Key_F3:
        return kVK_F3;
    case Qt::Key_F8:
        return kVK_F8;
    case Qt::Key_F9:
        return kVK_F9;
    case Qt::Key_F11:
        return kVK_F11;
    case Qt::Key_F13:
        return kVK_F13;
    case Qt::Key_F16:
        return kVK_F16;
    case Qt::Key_F14:
        return kVK_F14;
    case Qt::Key_F10:
        return kVK_F10;
    case Qt::Key_F12:
        return kVK_F12;
    case Qt::Key_F15:
        return kVK_F15;
    case Qt::Key_Help:
        return kVK_Help;
    case Qt::Key_Home:
        return kVK_Home;
    case Qt::Key_PageUp:
        return kVK_PageUp;
    case Qt::Key_Delete:
        return kVK_ForwardDelete;
    case Qt::Key_F4:
        return kVK_F4;
    case Qt::Key_End:
        return kVK_End;
    case Qt::Key_F2:
        return kVK_F2;
    case Qt::Key_PageDown:
        return kVK_PageDown;
    case Qt::Key_F1:
        return kVK_F1;
    case Qt::Key_Left:
        return kVK_LeftArrow;
    case Qt::Key_Right:
        return kVK_RightArrow;
    case Qt::Key_Down:
        return kVK_DownArrow;
    case Qt::Key_Up:
        return kVK_UpArrow;
    default:
        break;
    }

    UTF16Char keyCodeChar = keyCode;
    UCKeyboardTypeHeader* table = keyboard->keyboardTypeList;

    uint8_t* data = (uint8_t*)keyboard;
    for (quint32 i = 0; i < keyboard->keyboardTypeCount; i++) {
        UCKeyStateRecordsIndex* stateRec = 0;
        if (table[i].keyStateRecordsIndexOffset != 0) {
            stateRec = reinterpret_cast<UCKeyStateRecordsIndex*>(data + table[i].keyStateRecordsIndexOffset);
            if (stateRec->keyStateRecordsIndexFormat != kUCKeyStateRecordsIndexFormat) {
                stateRec = 0;
            }
        }

        UCKeyToCharTableIndex* charTable = reinterpret_cast<UCKeyToCharTableIndex*>(data + table[i].keyToCharTableIndexOffset);
        if (charTable->keyToCharTableIndexFormat != kUCKeyToCharTableIndexFormat) {
            continue;
        }

        for (quint32 j = 0; j < charTable->keyToCharTableCount; j++) {
            UCKeyOutput* keyToChar = reinterpret_cast<UCKeyOutput*>(data + charTable->keyToCharTableOffsets[j]);
            for (quint32 k = 0; k < charTable->keyToCharTableSize; k++) {
                if (keyToChar[k] & kUCKeyOutputTestForIndexMask) {
                    long idx = keyToChar[k] & kUCKeyOutputGetIndexMask;
                    if (stateRec && idx < stateRec->keyStateRecordCount) {
                        UCKeyStateRecord* rec = reinterpret_cast<UCKeyStateRecord*>(data + stateRec->keyStateRecordOffsets[idx]);
                        if (rec->stateZeroCharData == keyCodeChar) {
                            return k;
                        }
                    }
                } else if (!(keyToChar[k] & kUCKeyOutputSequenceIndexMask) && keyToChar[k] < 0xFFFE) {
                    if (keyToChar[k] == keyCodeChar) {
                        return k;
                    }
                }
            }
        }
    }

    return 0;
}

quint32 nativeModifiers(UCKeyboardLayout* keyboard, int key, Qt::KeyboardModifiers modifiers, quint32 keyNativeCode)
{
    quint32 result = 0;
    if (modifiers & Qt::ShiftModifier) {
        result |= shiftKey;
    }
    if (modifiers & Qt::ControlModifier) {
        result |= cmdKey;
    }
    if (modifiers & Qt::AltModifier) {
        result |= optionKey;
    }
    if (modifiers & Qt::MetaModifier) {
        result |= controlKey;
    }
    if (modifiers & Qt::KeypadModifier) {
        result |= kEventKeyModifierNumLockMask;
    }

    if (result == 0) {
        //! NOTE: Some symbols are available only through modifiers;
        //! if modifiers are not explicitly specified,
        //! we will get them through the native key

        //!NOTE: The algorithm below does not work correctly for this key
        if (key == Qt::Key_Plus) {
            return result;
        }

        UTF16Char keyCodeChar = key;

        int alt = (optionKey >> 8) & 0xff;
        UInt32 deadKeyState = 0;
        UniCharCount count;
        UniChar character;

        if (UCKeyTranslate(keyboard, keyNativeCode, kUCKeyActionDown, alt, 0, 0,
                           &deadKeyState, 1, &count, &character) == 0 && character == keyCodeChar) {
            result = optionKey;
        }
    }

    return result;
}

#ifdef MU_QT5_COMPAT
Qt::KeyboardModifiers qtModifiers(int keys)
{
    static QMap<int, Qt::KeyboardModifier> qtModifiers = {
        { shiftKey, Qt::ShiftModifier },
        { rightShiftKey, Qt::ShiftModifier },
        { controlKey, Qt::MetaModifier },
        { rightControlKey, Qt::MetaModifier },
        { cmdKey, Qt::ControlModifier },
        { optionKey, Qt::AltModifier },
        { rightOptionKey, Qt::AltModifier },
        { kEventKeyModifierNumLockMask, Qt::KeypadModifier },
        { 0, Qt::NoModifier }
    };

    Qt::KeyboardModifiers result = Qt::NoModifier;
    QMapIterator<int, Qt::KeyboardModifier> it(qtModifiers);
    while (it.hasNext()) {
        it.next();

        if (keys & it.key()) {
            result |= it.value();
        }
    }

    return result;
}

QSet<int> possibleKeys(const QKeySequence& sequence)
{
    const int key = sequence[0];

    Qt::Key qKey = Qt::Key(key & ~Qt::KeyboardModifierMask);
    if (qKey == Qt::Key_Insert) {
        return { key };
    }

    UCKeyboardLayout* keyboard = keyboardLayout();
    if (!keyboard) {
        LOGE() << "The keyboard layout is not valid";
        return {};
    }

    Qt::KeyboardModifiers modifiers = Qt::KeyboardModifiers(key & Qt::KeyboardModifierMask);

    quint32 keyNativeCode = nativeKeycode(keyboard, qKey);
    quint32 keyNativeModifiers = nativeModifiers(keyboard, qKey, modifiers, keyNativeCode);

    //! NOTE: It may be that we resolved modifiers through the native key,
    //! then we should update the qt modifiers
    modifiers = qtModifiers(keyNativeModifiers);

    QKeyEvent fakeKey(QKeyEvent::None, key, modifiers, keyNativeCode, keyNativeCode, keyNativeModifiers);
    QList<int> keys = QGuiApplicationPrivate::platformIntegration()->possibleKeys(&fakeKey);

    QList<int> result;
    for (int key : keys) {
        if (modifiers != Qt::NoModifier) {
            if (Qt::KeyboardModifiers(key & Qt::KeyboardModifierMask) == Qt::NoModifier) {
                key += modifiers;
            }

            if (Qt::KeyboardModifiers(key & Qt::KeyboardModifierMask) != modifiers) {
                continue;
            }
        }

        result << key;
    }

    return QSet<int>(result.cbegin(), result.cend());
}

MacOSShortcutsInstanceModel::MacOSShortcutsInstanceModel(QObject* parent)
    : ShortcutsInstanceModel(parent)
{
    connect(qApp->inputMethod(), &QInputMethod::localeChanged, this, [this]() {
        doLoadShortcuts();
    });
}

void MacOSShortcutsInstanceModel::doLoadShortcuts()
{
    m_shortcuts.clear();
    m_shortcutMap.clear();

    ShortcutList shortcuts = shortcutsRegister()->shortcuts();

    for (const Shortcut& sc : shortcuts) {
        for (const std::string& seq : sc.sequences) {
            QString sequence = QString::fromStdString(seq);

            QSet<int> keys = possibleKeys(QKeySequence::fromString(sequence, QKeySequence::PortableText));
            for (int key : keys) {
                QKeySequence keySeq(key);
                QString seqStr = keySeq.toString(QKeySequence::PortableText);

                //! NOTE There may be several identical shortcuts for different contexts.
                //! We only need a list of unique ones.
                if (!m_shortcuts.contains(seqStr)) {
                    m_shortcuts << seqStr;
                    m_shortcutMap.insert(seqStr, sequence);
                }
            }
        }
    }

    emit shortcutsChanged();
}

void MacOSShortcutsInstanceModel::doActivate(const QString& key)
{
    ShortcutsInstanceModel::doActivate(m_shortcutMap.value(key));
}

#else // MU_QT5_COMPAT

Qt::KeyboardModifiers qtModifiers(int keys)
{
    static QMap<int, Qt::KeyboardModifier> qtModifiers = {
        { shiftKey, Qt::ShiftModifier },
        { rightShiftKey, Qt::ShiftModifier },
        { controlKey, Qt::MetaModifier },
        { rightControlKey, Qt::MetaModifier },
        { cmdKey, Qt::ControlModifier },
        { optionKey, Qt::AltModifier },
        { rightOptionKey, Qt::AltModifier },
        { kEventKeyModifierNumLockMask, Qt::KeypadModifier },
        { 0, Qt::NoModifier }
    };

    Qt::KeyboardModifiers result = Qt::NoModifier;
    QMapIterator<int, Qt::KeyboardModifier> it(qtModifiers);
    while (it.hasNext()) {
        it.next();

        if (keys & it.key()) {
            result |= it.value();
        }
    }

    return result;
}

QSet<QKeyCombination> possibleKeys(const QKeySequence& sequence)
{
    const QKeyCombination keyCombination = sequence[0];

    const Qt::Key qKey = keyCombination.key();
    if (qKey == Qt::Key_Insert) {
        return { keyCombination };
    }

    UCKeyboardLayout* keyboard = keyboardLayout();
    if (!keyboard) {
        LOGE() << "The keyboard layout is not valid";
        return {};
    }

    Qt::KeyboardModifiers modifiers = keyCombination.keyboardModifiers();

    quint32 keyNativeCode = nativeKeycode(keyboard, qKey);
    quint32 keyNativeModifiers = nativeModifiers(keyboard, qKey, modifiers, keyNativeCode);

    //! NOTE: It may be that we resolved modifiers through the native key,
    //! then we should update the qt modifiers
    modifiers = qtModifiers(keyNativeModifiers);

    QKeyEvent fakeKey(QKeyEvent::None, keyCombination.toCombined(), modifiers, keyNativeCode, keyNativeCode, keyNativeModifiers);
    QList<int> keys = QGuiApplicationPrivate::platformIntegration()->possibleKeys(&fakeKey);

    QSet<QKeyCombination> result;
    for (int key : keys) {
        QKeyCombination combination = QKeyCombination::fromCombined(key);
        if (modifiers != Qt::NoModifier) {
            if (combination.keyboardModifiers() == Qt::NoModifier) {
                combination = QKeyCombination(modifiers, combination.key());
            }

            if (combination.keyboardModifiers() != modifiers) {
                continue;
            }
        }

        result << combination;
    }

    return result;
}

MacOSShortcutsInstanceModel::MacOSShortcutsInstanceModel(QObject* parent)
    : ShortcutsInstanceModel(parent)
{
    connect(qApp->inputMethod(), &QInputMethod::localeChanged, this, [this]() {
        doLoadShortcuts();
    });
}

void MacOSShortcutsInstanceModel::doLoadShortcuts()
{
    m_shortcuts.clear();
    m_shortcutMap.clear();

    ShortcutList shortcuts = shortcutsRegister()->shortcuts();

    for (const Shortcut& sc : shortcuts) {
        for (const std::string& seq : sc.sequences) {
            QString sequence = QString::fromStdString(seq);

            QSet<QKeyCombination> keys = possibleKeys(QKeySequence::fromString(sequence, QKeySequence::PortableText));
            for (QKeyCombination key : keys) {
                QKeySequence keySeq(key);
                QString seqStr = keySeq.toString(QKeySequence::PortableText);

                //! NOTE There may be several identical shortcuts for different contexts.
                //! We only need a list of unique ones.
                if (!m_shortcuts.contains(seqStr)) {
                    m_shortcuts << seqStr;
                    m_shortcutMap.insert(seqStr, sequence);
                }
            }
        }
    }

    emit shortcutsChanged();
}

void MacOSShortcutsInstanceModel::doActivate(const QString& key)
{
    ShortcutsInstanceModel::doActivate(m_shortcutMap.value(key));
}

#endif // else MU_QT5_COMPAT
