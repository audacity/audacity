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

TEST_F(EffectsBase_ConfigTests, ReadWrite)
{
    EffectConfigSettings config("config_test.cfg");
    config.Clear();

    config.Write("group1/sub1/sub2/param1", wxString("val1"));
    config.Write("group1/sub1/sub3/param1", 42);

    wxString val1;
    bool ok = config.Read("group1/sub1/sub2/param1", &val1);
    EXPECT_TRUE(ok);
    EXPECT_TRUE(val1 == "val1");

    int val2;
    ok = config.Read("group1/sub1/sub3/param1", &val2);
    EXPECT_TRUE(ok);
    EXPECT_TRUE(val2 == 42);
}

TEST_F(EffectsBase_ConfigTests, SaveLoad)
{
    // save
    {
        EffectConfigSettings config("config_test.cfg");
        config.Clear();

        config.Write("group1/sub1/sub2/param1", wxString("val1"));
        config.Write("group1/sub1/sub3/param1", 42);
    }

    // load
    {
        EffectConfigSettings config("config_test.cfg");
        wxString val1;
        bool ok = config.Read("group1/sub1/sub2/param1", &val1);
        EXPECT_TRUE(ok);
        EXPECT_TRUE(val1 == "val1");

        int val2;
        ok = config.Read("group1/sub1/sub3/param1", &val2);
        EXPECT_TRUE(ok);
        EXPECT_TRUE(val2 == 42);
    }
}

TEST_F(EffectsBase_ConfigTests, GetGroup)
{
    EffectConfigSettings config("config_test.cfg");
    config.Clear();

    EXPECT_TRUE(config.GetGroup() == "General");

    {
        auto scope = config.BeginGroup("group1");
        EXPECT_TRUE(config.GetGroup() == "group1");
    }

    EXPECT_TRUE(config.GetGroup() == "General");
}

TEST_F(EffectsBase_ConfigTests, GetChildGroups)
{
    EffectConfigSettings config("config_test.cfg");
    config.Clear();

    config.Write("group1/sub1/sub2/param1", wxString("val1"));
    config.Write("group1/sub1/sub3/param1", wxString("val2"));
    config.Write("group1/sub1/param1", wxString("val3"));

    auto groups = config.GetChildGroups();
    EXPECT_TRUE(groups.size() == 3);
    EXPECT_TRUE(groups.Index("group1/sub1/sub2") != -1);
    EXPECT_TRUE(groups.Index("group1/sub1/sub3") != -1);
    EXPECT_TRUE(groups.Index("group1/sub1") != -1);

    auto scope = config.BeginGroup("group1/sub1");
    groups = config.GetChildGroups();
    EXPECT_TRUE(groups.size() == 2);
    EXPECT_TRUE(groups.Index("sub2") != -1);
    EXPECT_TRUE(groups.Index("sub3") != -1);
}

TEST_F(EffectsBase_ConfigTests, GetChildKeys)
{
    EffectConfigSettings config("config_test.cfg");
    config.Clear();

    config.Write("group1/sub1/sub2/param1", wxString("val1"));
    config.Write("group1/sub1/sub3/param2", wxString("val2"));
    config.Write("group1/sub1/param3", wxString("val3"));

    auto keys = config.GetChildKeys();
    EXPECT_TRUE(keys.size() == 0);

    auto scope = config.BeginGroup("group1/sub1");
    keys = config.GetChildKeys();
    EXPECT_TRUE(keys.size() == 1);
    EXPECT_TRUE(keys.Index("param3") != -1);
}

TEST_F(EffectsBase_ConfigTests, HasEntry)
{
    EffectConfigSettings config("config_test.cfg");
    config.Clear();

    config.Write("group1/sub1/sub2/param1", wxString("val1"));
    config.Write("group1/sub1/sub3/param2", wxString("val2"));
    config.Write("group1/sub1/param3", wxString("val3"));

    EXPECT_TRUE(config.HasEntry("group1/sub1/sub2/param1"));
    EXPECT_FALSE(config.HasEntry("group1/sub1/sub2/param42"));
}

TEST_F(EffectsBase_ConfigTests, HasGroup)
{
    EffectConfigSettings config("config_test.cfg");
    config.Clear();

    config.Write("group1/sub1/sub2/param1", wxString("val1"));
    config.Write("group1/sub1/sub3/param2", wxString("val2"));
    config.Write("group1/sub1/param3", wxString("val3"));

    EXPECT_TRUE(config.HasGroup("group1/sub1/sub2"));
    EXPECT_TRUE(config.HasGroup("group1/sub1"));
    EXPECT_FALSE(config.HasGroup("group1/sub1/sub42"));
}
