/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "au3-import-export/ExportPlugin.h"
#include "au3-import-export/ExportPluginRegistry.h"

#include "importexport/export/internal/exportconfiguration.h"
#include "importexport/export/types/exporttypes.h"

using namespace au::importexport;

TEST(ExportDefaultsTests, DefaultFormatIsRegistered)
{
    bool found = false;
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.msgid().toStdString() == DEFAULT_EXPORT_FORMAT) {
            found = true;
            break;
        }
    }

    EXPECT_TRUE(found);
}

TEST(ExportDefaultsTests, ConfigurationDefaultsToTheDefaultFormat)
{
    ExportConfiguration configuration;
    configuration.init();

    EXPECT_EQ(configuration.currentFormat(), DEFAULT_EXPORT_FORMAT);
}
