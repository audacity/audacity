/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/ExportPlugin.h"

class ExportOGG final : public ExportPlugin
{
public:

    ExportOGG();

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int) const override;

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;
};
