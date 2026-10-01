/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/ExportPlugin.h"

class ExportMP2 final : public ExportPlugin
{
public:

    ExportMP2();

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int) const override;

    // Required

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int) const override;
};
