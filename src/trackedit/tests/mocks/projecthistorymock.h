/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "trackedit/iprojecthistory.h"

namespace au::trackedit {
class ProjectHistoryMock : public IProjectHistory
{
public:
    MOCK_METHOD(void, init, (), (override));

    MOCK_METHOD(bool, undoAvailable, (), (const, override));
    MOCK_METHOD(void, undo, (), (override));
    MOCK_METHOD(bool, redoAvailable, (), (const, override));
    MOCK_METHOD(void, redo, (), (override));
    MOCK_METHOD(void, pushHistoryState, (const std::string& longDescription, const std::string& shortDescription), (override));
    MOCK_METHOD(void, pushHistoryState, (const std::string& longDescription, const std::string& shortDescription,
                                         trackedit::UndoPushType flags), (override));

    MOCK_METHOD(void, undoRedoToIndex, (size_t index), (override));

    MOCK_METHOD(const muse::TranslatableString, topMostUndoActionName, (), (const, override));
    MOCK_METHOD(const muse::TranslatableString, topMostRedoActionName, (), (const, override));
    MOCK_METHOD(size_t, undoRedoActionCount, (), (const, override));
    MOCK_METHOD(size_t, currentStateIndex, (), (const, override));
    MOCK_METHOD(const muse::TranslatableString, lastActionNameAtIdx, (size_t idx), (const, override));

    MOCK_METHOD(muse::async::Notification, historyChanged, (), (const, override));

    MOCK_METHOD(void, rollbackState, (), (override));
    MOCK_METHOD(void, modifyState, (bool autoSave), (override));
    MOCK_METHOD(void, markUnsaved, (), (override));

    MOCK_METHOD(void, startUserInteraction, (), (override));
    MOCK_METHOD(void, endUserInteraction, (), (override));
};
}
