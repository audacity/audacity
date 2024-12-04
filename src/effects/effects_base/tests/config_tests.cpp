/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "../internal/effectconfigsettings.h"

#include "log.h"

using namespace au::au3;

class EffectsBase_ConfigTests : public ::testing::Test
{
public:
};

TEST_F(EffectsBase_ConfigTests, All)
{
    EffectConfigSettings config("config_test.cfg");

    //std::string key = "/pluginsettings/base64:RWZmZWN0X0F1ZGFjaXR5X0F1ZGFjaXR5X1JldmVyYg==/private/FactoryDefaults/Parameters";
    //std::string key2 = "/pluginsettings/private/FactoryDefaults/Parameters";
    std::string key3 = "/pluginsettings_base64:RWZmZWN0X0F1ZGFjaXR5X0F1ZGFjaXR5X1JldmVyYg==_private_FactoryDefaults/Parameters";
    std::string val = "Delay=\"10\" DryGain=\"-1\"";

    config.Write("group1/sub1/sub2/param1", "val1");
    config.Write("group1/sub1/sub2/param2", "val2");

    bool isHasGroup = config.HasGroup("group1/sub1/sub2");
    EXPECT_TRUE(isHasGroup);
}
