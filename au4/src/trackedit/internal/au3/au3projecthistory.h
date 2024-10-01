/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "iprojecthistory.h"

#include "context/iglobalcontext.h"
#include "modularity/ioc.h"

class AudacityProject;

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
    void undoUnsaved() override;
    void clearUnsaved() override;
    void pushHistoryState(
        const std::string& longDescription, const std::string& shortDescription) override;

private:
    ::AudacityProject& projectRef();
};
}
