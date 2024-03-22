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
#include "shortcutsregister.h"

#include <QKeySequence>

#include "global/deprecated/xmlreader.h"
#include "global/deprecated/xmlwriter.h"
#include "multiinstances/resourcelockguard.h"

#include "log.h"

using namespace mu::shortcuts;
using namespace mu::async;

static constexpr std::string_view SHORTCUTS_TAG("Shortcuts");
static constexpr std::string_view SHORTCUT_TAG("SC");
static constexpr std::string_view ACTION_CODE_TAG("key");
static constexpr std::string_view STANDARD_KEY_TAG("std");
static constexpr std::string_view SEQUENCE_TAG("seq");

static const std::string SHORTCUTS_RESOURCE_NAME("SHORTCUTS");

static const Shortcut& findShortcut(const ShortcutList& shortcuts, const std::string& actionCode)
{
    for (const Shortcut& shortcut: shortcuts) {
        if (shortcut.action == actionCode) {
            return shortcut;
        }
    }

    static Shortcut null;
    return null;
}

void ShortcutsRegister::init()
{
    multiInstancesProvider()->resourceChanged().onReceive(this, [this](const std::string& resourceName) {
        if (resourceName == SHORTCUTS_RESOURCE_NAME) {
            reload();
        }
    });

    reload();
}

void ShortcutsRegister::reload(bool onlyDef)
{
    TRACEFUNC;

    m_shortcuts.clear();
    m_defaultShortcuts.clear();

    io::path_t defPath = configuration()->shortcutsAppDataPath();
    io::path_t userPath = configuration()->shortcutsUserAppDataPath();

    bool ok = readFromFile(m_defaultShortcuts, defPath);

    if (ok) {
        expandStandardKeys(m_defaultShortcuts);

        if (!onlyDef) {
            //! NOTE The user shortcut file may change, so we need to lock it
            mi::ReadResourceLockGuard(multiInstancesProvider(), SHORTCUTS_RESOURCE_NAME);
            ok = readFromFile(m_shortcuts, userPath);
        } else {
            ok = false;
        }

        if (!ok) {
            m_shortcuts = m_defaultShortcuts;
        } else {
            mergeShortcuts(m_shortcuts, m_defaultShortcuts);
            mergeAdditionalShortcuts(m_shortcuts);
        }

        ok = true;
    }

    if (ok) {
        expandStandardKeys(m_shortcuts);
        makeUnique(m_shortcuts);
        m_shortcutsChanged.notify();
    }
}

void ShortcutsRegister::mergeShortcuts(ShortcutList& shortcuts, const ShortcutList& defaultShortcuts) const
{
    TRACEFUNC;

    ShortcutList needadd;
    for (const Shortcut& defSc : defaultShortcuts) {
        Shortcut scForAdd = defSc;
        bool found = false;

        for (Shortcut& sc : shortcuts) {
            //! NOTE If user shortcut is found, set context (context should always as default)
            if (sc.action == defSc.action) {
                sc.context = defSc.context;
                found = true;
            } else if (sc.context == defSc.context) {
                for (const std::string& seq : sc.sequences) {
                    //! NOTE If user shortcut has sequence from default shortcut, remove the sequence from default shortcut
                    mu::remove_if(scForAdd.sequences, [&seq](const std::string& cmp){
                        return cmp == seq;
                    });
                }
            }
        }

        //! NOTE If no default shortcut is found in user shortcuts add def
        if (!found) {
            needadd.push_back(scForAdd);
        }
    }

    if (!needadd.empty()) {
        shortcuts.splice(shortcuts.end(), needadd);
    }
}

void ShortcutsRegister::mergeAdditionalShortcuts(ShortcutList& shortcuts)
{
    for (const auto& [context, additionalShortcuts] : m_additionalShortcutsMap) {
        mergeShortcuts(shortcuts, additionalShortcuts);
    }
}

void ShortcutsRegister::makeUnique(ShortcutList& shortcuts)
{
    TRACEFUNC;

    const ShortcutList all = shortcuts;

    shortcuts.clear();

    for (const Shortcut& sc : all) {
        const std::string& action = sc.action;

        auto it = std::find_if(shortcuts.begin(), shortcuts.end(), [action](const Shortcut& s) {
            return s.action == action;
        });

        if (it == shortcuts.end()) {
            shortcuts.push_back(sc);
            continue;
        }

        Shortcut& foundSc = *it;

        IF_ASSERT_FAILED(foundSc.context == sc.context) {
        }

        foundSc.sequences.insert(foundSc.sequences.end(), sc.sequences.begin(), sc.sequences.end());
    }
}

void ShortcutsRegister::expandStandardKeys(ShortcutList& shortcuts) const
{
    TRACEFUNC;

    ShortcutList expanded;
    ShortcutList notbonded;

    for (Shortcut& shortcut : shortcuts) {
        if (!shortcut.sequences.empty()) {
            continue;
        }

        QList<QKeySequence> kslist = QKeySequence::keyBindings(shortcut.standardKey);
        if (kslist.isEmpty()) {
            notbonded.push_back(shortcut);
            continue;
        }

        const QKeySequence& first = kslist.first();
        shortcut.sequences.push_back(first.toString().toStdString());
        //LOGD() << "for standard key: " << sc.standardKey << ", sequence: " << sc.sequence;

        //! NOTE If the keyBindings contains more than one result,
        //! these can be considered alternative shortcuts on the same platform for the given key.
        for (int i = 1; i < kslist.count(); ++i) {
            const QKeySequence& seq = kslist.at(i);
            Shortcut esc = shortcut;
            esc.sequences = { seq.toString().toStdString() };
            //LOGD() << "for standard key: " << esc.standardKey << ", alternative sequence: " << esc.sequence;
            expanded.push_back(esc);
        }
    }

    if (!notbonded.empty()) {
        LOGD() << "removed " << notbonded.size() << " shortcut, because they are not bound to standard key";
        for (const Shortcut& sc : notbonded) {
            shortcuts.remove(sc);
        }
    }

    if (!expanded.empty()) {
        LOGD() << "added " << expanded.size() << " shortcut, because they are alternative shortcuts for the given standard keys";

        shortcuts.splice(shortcuts.end(), expanded);
    }
}

ShortcutList ShortcutsRegister::filterAndUpdateAdditionalShortcuts(const ShortcutList& shortcuts)
{
    ShortcutList noAdditionalShortcuts = shortcuts;

    for (auto& [context, additionalShortcuts] : m_additionalShortcutsMap) {
        for (Shortcut& shortcut : additionalShortcuts) {
            auto it = std::find(shortcuts.begin(), shortcuts.end(), shortcut.action);
            if (it != shortcuts.end()) {
                shortcut = *it;
                noAdditionalShortcuts.remove(shortcut);
            }
        }
    }

    return noAdditionalShortcuts;
}

bool ShortcutsRegister::readFromFile(ShortcutList& shortcuts, const io::path_t& path) const
{
    TRACEFUNC;

    deprecated::XmlReader reader(path);

    reader.readNextStartElement();
    if (reader.tagName() != SHORTCUTS_TAG) {
        return false;
    }

    while (reader.readNextStartElement()) {
        if (reader.tagName() != SHORTCUT_TAG) {
            reader.skipCurrentElement();
            continue;
        }

        Shortcut shortcut = readShortcut(reader);
        if (shortcut.isValid()) {
            shortcuts.push_back(shortcut);
        }
    }

    if (!reader.success()) {
        LOGE() << "failed parse xml, error: " << reader.error() << ", path: " << path;
    }

    return reader.success();
}

Shortcut ShortcutsRegister::readShortcut(deprecated::XmlReader& reader) const
{
    Shortcut shortcut;

    while (reader.readNextStartElement()) {
        std::string tag(reader.tagName());

        if (tag == ACTION_CODE_TAG) {
            shortcut.action = reader.readString();
        } else if (tag == STANDARD_KEY_TAG) {
            shortcut.standardKey = QKeySequence::StandardKey(reader.readInt());
        } else if (tag == SEQUENCE_TAG) {
            shortcut.sequences.push_back(reader.readString());
        } else {
            reader.skipCurrentElement();
        }
    }

    shortcut.context = uiactionsRegister()->action(shortcut.action).scCtx;

    return shortcut;
}

const ShortcutList& ShortcutsRegister::shortcuts() const
{
    return m_shortcuts;
}

mu::Ret ShortcutsRegister::setShortcuts(const ShortcutList& shortcuts)
{
    TRACEFUNC;

    if (shortcuts == m_shortcuts) {
        return true;
    }

    ShortcutList needToWrite = filterAndUpdateAdditionalShortcuts(shortcuts);

    bool ok = writeToFile(needToWrite, configuration()->shortcutsUserAppDataPath());

    if (ok) {
        m_shortcuts = needToWrite;
        mergeShortcuts(m_shortcuts, m_defaultShortcuts);
        mergeAdditionalShortcuts(m_shortcuts);
        m_shortcutsChanged.notify();
    }

    return ok;
}

void ShortcutsRegister::resetShortcuts()
{
    mi::WriteResourceLockGuard(multiInstancesProvider(), SHORTCUTS_RESOURCE_NAME);
    fileSystem()->remove(configuration()->shortcutsUserAppDataPath());

    reload();
}

bool ShortcutsRegister::writeToFile(const ShortcutList& shortcuts, const io::path_t& path) const
{
    TRACEFUNC;

    mi::WriteResourceLockGuard(multiInstancesProvider(), SHORTCUTS_RESOURCE_NAME);

    deprecated::XmlWriter writer(path);

    writer.writeStartDocument();
    writer.writeStartElement(SHORTCUTS_TAG);

    for (const Shortcut& shortcut : shortcuts) {
        writeShortcut(writer, shortcut);
    }

    writer.writeEndElement();
    writer.writeEndDocument();

    return writer.success();
}

void ShortcutsRegister::writeShortcut(deprecated::XmlWriter& writer, const Shortcut& shortcut) const
{
    writer.writeStartElement(SHORTCUT_TAG);
    writer.writeTextElement(ACTION_CODE_TAG, shortcut.action);

    if (shortcut.standardKey != QKeySequence::UnknownKey) {
        writer.writeTextElement(STANDARD_KEY_TAG, QString("%1").arg(shortcut.standardKey).toStdString());
    }

    for (const std::string& seq : shortcut.sequences) {
        writer.writeTextElement(SEQUENCE_TAG, seq);
    }

    writer.writeEndElement();
}

Notification ShortcutsRegister::shortcutsChanged() const
{
    return m_shortcutsChanged;
}

mu::Ret ShortcutsRegister::setAdditionalShortcuts(const std::string& context, const ShortcutList& shortcuts)
{
    m_additionalShortcutsMap[context] = shortcuts;

    mergeShortcuts(m_shortcuts, m_additionalShortcutsMap[context]);
    m_shortcutsChanged.notify();

    return make_ok();
}

const Shortcut& ShortcutsRegister::shortcut(const std::string& actionCode) const
{
    return findShortcut(m_shortcuts, actionCode);
}

const Shortcut& ShortcutsRegister::defaultShortcut(const std::string& actionCode) const
{
    return findShortcut(m_defaultShortcuts, actionCode);
}

bool ShortcutsRegister::isRegistered(const std::string& sequence) const
{
    for (const Shortcut& sh : m_shortcuts) {
        auto it = std::find(sh.sequences.cbegin(), sh.sequences.cend(), sequence);
        if (it != sh.sequences.cend()) {
            return true;
        }
    }
    return false;
}

ShortcutList ShortcutsRegister::shortcutsForSequence(const std::string& sequence) const
{
    ShortcutList list;
    for (const Shortcut& sh : m_shortcuts) {
        auto it = std::find(sh.sequences.cbegin(), sh.sequences.cend(), sequence);
        if (it != sh.sequences.cend()) {
            list.push_back(sh);
        }
    }
    return list;
}

mu::Ret ShortcutsRegister::importFromFile(const io::path_t& filePath)
{
    mi::ReadResourceLockGuard(multiInstancesProvider(), SHORTCUTS_RESOURCE_NAME);

    Ret ret = fileSystem()->copy(filePath, configuration()->shortcutsUserAppDataPath(), true);
    if (!ret) {
        LOGE() << "failed import file: " << ret.toString();
        return ret;
    }

    reload();

    return make_ret(Ret::Code::Ok);
}

mu::Ret ShortcutsRegister::exportToFile(const io::path_t& filePath) const
{
    return writeToFile(m_shortcuts, filePath);
}

bool ShortcutsRegister::active()
{
    return m_isActive;
}

void ShortcutsRegister::setActive(bool active)
{
    if (m_isActive == active) {
        return;
    }

    m_isActive = active;
    m_activeChanged.notify();
}

Notification ShortcutsRegister::activeChanged() const
{
    return m_activeChanged;
}
