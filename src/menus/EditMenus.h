#pragma once

#include "commands/CommandFunctors.h"

namespace EditActions {

// Menu handler functions
struct Handler : CommandHandlerObject {

    void OnUndo(const CommandContext &context);
    void OnRedo(const CommandContext &context);
    void OnCut(const CommandContext &context);
    void OnDelete(const CommandContext &context);
    void OnCopy(const CommandContext &context);
    void OnPaste(const CommandContext &context);
    void OnDuplicate(const CommandContext &context);
    void OnSplitCut(const CommandContext &context);
    void OnSplitDelete(const CommandContext &context);
    void OnSilence(const CommandContext &context);
    void OnTrim(const CommandContext &context);
    void OnSplit(const CommandContext &context);
    void OnSplitNew(const CommandContext &context);
    void OnJoin(const CommandContext &context);
    void OnDisjoin(const CommandContext &context);
    void OnEditMetadata(const CommandContext &context);
    void OnPreferences(const CommandContext &context);
};
}
