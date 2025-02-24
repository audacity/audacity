/*
 * Audacity: A Digital Audio Editor
 */
#include "selectionrestorer.h"

#include "libraries/lib-project-history/UndoManager.h"
#include "libraries/lib-project/Project.h"

namespace au::trackedit {
static const ::AudacityProject::AttachedObjects::RegisteredFactory key{
    [](AudacityProject&)
    {
        return std::make_shared<SelectionRestorer>();
    } };

SelectionRestorer& SelectionRestorer::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<SelectionRestorer>(key);
}

class ClipAndTimeSelectionState final : public UndoStateExtension
{
public:
    ClipAndTimeSelectionState(AudacityProject& project)
        : m_selection(SelectionRestorer::Get(project).selectionGetter())
    {
    }

private:
    void RestoreUndoRedoState(AudacityProject& project) override
    {
        SelectionRestorer::Get(project).selectionSetter(m_selection);
    }

    const ClipAndTimeSelection m_selection;
};

UndoRedoExtensionRegistry::Entry sEntry {
    [](AudacityProject& project) -> std::shared_ptr<UndoStateExtension> {
        return std::make_shared<ClipAndTimeSelectionState>(project);
    }
};
}
