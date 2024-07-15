/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "iprojecthistory.h"

#include "async/asyncable.h"
#include "context/iglobalcontext.h"
#include "modularity/ioc.h"

namespace au::trackedit {
class Au3ProjectHistory : public IProjectHistory
{
    muse::Inject<context::IGlobalContext> globalContext;

public:
    void init() override;

    bool undoAvailable() override;
    void undo() override;
    bool redoAvailable() override;
    void redo() override;

    void pushHistoryState(const std::string& longDescription, const std::string& shortDescription) override;
};
}
