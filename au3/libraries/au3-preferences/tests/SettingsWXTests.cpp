#include <catch2/catch.hpp>
#include "SettingsTestsCommon.h"
#include <wx/fileconf.h>
#include "SettingsWX.h"

TEST_CASE("SettingsWX implementations test", "")
{
    std::shared_ptr<wxConfigBase> config
        = std::make_unique<wxFileConfig>(wxEmptyString,
                                         wxEmptyString,
                                         "wx-settings-test.cfg",
                                         wxEmptyString,
                                         wxCONFIG_USE_LOCAL_FILE | wxCONFIG_USE_RELATIVE_PATH);

    wxConfigBase::Set(config.get());

    audacity::ApplicationSettings::Scope testScope {
        [&] { return std::make_unique<SettingsWX>(config); }
    };

    SECTION("Application settings read/write")
    {
        auto settings = audacity::ApplicationSettings::Call();
        TestRW(*settings);
    }

    SECTION("Application settings groups")
    {
        auto settings = audacity::ApplicationSettings::Call();
        TestGroups(*settings);
    }

    SECTION("Application settings persistance")
    {
        {
            auto settings = audacity::ApplicationSettings::Call();
            settings->Clear();

            settings->Write("Application Settings/Property1", "string");
            settings->Write("Application Settings/Component1/Property1", 1);
            settings->Write("Application Settings/Component1/Property2", true);
            settings->Write("Application Settings/Component2/Property1", 3.0);
        }

        {
            auto settings = audacity::ApplicationSettings::Call();
            REQUIRE(settings->Read("Application Settings/Property1") == "string");
            REQUIRE(settings->Read("Application Settings/Component1/Property1", 0) == 1);
            REQUIRE(settings->Read("Application Settings/Component1/Property2", false) == true);
            REQUIRE(settings->Read("Application Settings/Component2/Property1", 0.0) == 3.0);
        }
    }
}
