#pragma once

#include "commands/CommandFunctors.h"

namespace FileActions {

// Menu handler functions
struct Handler : CommandHandlerObject {

    void OnNew(const CommandContext & );
    void OnOpen(const CommandContext &context );
    void OnProjectReset(const CommandContext &context);
    void OnClose(const CommandContext &context );
    void OnCompact(const CommandContext &context);
    void OnSave(const CommandContext &context );
    void OnSaveAs(const CommandContext &context );
    void OnSaveCopy(const CommandContext &context );
    void OnExportMp3(const CommandContext &context);
    void OnExportWav(const CommandContext &context);
    void OnExportOgg(const CommandContext &context);
    void OnExportAudio(const CommandContext &context);
    void OnExportSelection(const CommandContext &context);
    void OnExportLabels(const CommandContext &context);
    void OnExportMultiple(const CommandContext &context);
#ifdef USE_MIDI
    void OnExportMIDI(const CommandContext &context);
#endif
    void OnImport(const CommandContext &context);
    void OnImportLabels(const CommandContext &context);
#ifdef USE_MIDI
    void OnImportMIDI(const CommandContext &context);
#endif
    void OnImportRaw(const CommandContext &context);
    void OnPageSetup(const CommandContext &context);
    void OnPrint(const CommandContext &context);
    void OnExit(const CommandContext&);
    void OnExportFLAC(const CommandContext &context);
};
}
