/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <QTemporaryDir>

#include "global/modularity/ioc.h"
#include "global/iglobalconfiguration.h"
#include "global/tests/mocks/globalconfigurationmock.h"

#include "macromanager/internal/macrosrepository.h"

using namespace muse;
using namespace au::macromanager;

using ::testing::NiceMock;
using ::testing::Return;

class MacroManager_MacrosRepositoryTests : public ::testing::Test
{
protected:
    static constexpr const char* MODULE = "macromanager_tests";

    void SetUp() override
    {
        ASSERT_TRUE(m_directory.isValid());

        m_globalConfiguration = std::make_shared<NiceMock<GlobalConfigurationMock> >();
        ON_CALL(*m_globalConfiguration, userAppDataPath()).WillByDefault(Return(io::path_t(m_directory.path())));
        ON_CALL(*m_globalConfiguration, appDataPath()).WillByDefault(Return(io::path_t(m_directory.path())));

        modularity::globalIoc()->unregister<IGlobalConfiguration>(MODULE);
        modularity::globalIoc()->registerExport<IGlobalConfiguration>(MODULE, m_globalConfiguration);

        m_repository = makeRepository();
    }

    void TearDown() override
    {
        modularity::globalIoc()->unregister<IGlobalConfiguration>(MODULE);
    }

    static std::shared_ptr<MacrosRepository> makeRepository()
    {
        auto repository = std::make_shared<MacrosRepository>();
        repository->init();
        return repository;
    }

    static std::vector<std::string> names(const MacrosRepository& repository)
    {
        std::vector<std::string> result;
        for (const Macro& macro : repository.macros()) {
            result.push_back(macro.name);
        }
        return result;
    }

    QTemporaryDir m_directory;
    std::shared_ptr<GlobalConfigurationMock> m_globalConfiguration;
    std::shared_ptr<MacrosRepository> m_repository;
};

TEST_F(MacroManager_MacrosRepositoryTests, SaveKeepsMacrosWhoseNamesEscapeToTheSameFileName)
{
    ASSERT_TRUE(m_repository->save(Macro { "my_macro", {} }));
    ASSERT_TRUE(m_repository->save(Macro { "my macro", {} }));

    EXPECT_THAT(names(*m_repository), ::testing::UnorderedElementsAre("my_macro", "my macro"));
    EXPECT_THAT(names(*makeRepository()), ::testing::UnorderedElementsAre("my_macro", "my macro"));
}

TEST_F(MacroManager_MacrosRepositoryTests, SaveOverwritesTheSameMacro)
{
    ASSERT_TRUE(m_repository->save(Macro { "my macro", {} }));
    ASSERT_TRUE(m_repository->save(Macro { "my macro", { MacroReference { "other" } } }));

    const std::shared_ptr<MacrosRepository> reloaded = makeRepository();
    ASSERT_THAT(names(*reloaded), ::testing::ElementsAre("my macro"));
    EXPECT_EQ(reloaded->macro("my macro")->steps.size(), 1);
}

TEST_F(MacroManager_MacrosRepositoryTests, RenameDoesNotOverwriteMacroWhoseNameEscapesToTheSameFileName)
{
    ASSERT_TRUE(m_repository->save(Macro { "a_b", {} }));
    ASSERT_TRUE(m_repository->save(Macro { "x", {} }));

    ASSERT_TRUE(m_repository->rename("x", "a b"));

    EXPECT_THAT(names(*m_repository), ::testing::UnorderedElementsAre("a_b", "a b"));
    EXPECT_THAT(names(*makeRepository()), ::testing::UnorderedElementsAre("a_b", "a b"));
}

TEST_F(MacroManager_MacrosRepositoryTests, RenameUpdatesReferencesInDependentMacros)
{
    ASSERT_TRUE(m_repository->save(Macro { "child", {} }));
    ASSERT_TRUE(m_repository->save(Macro { "parent", { MacroReference { "child" } } }));

    ASSERT_TRUE(m_repository->rename("child", "kid"));

    const std::shared_ptr<MacrosRepository> reloaded = makeRepository();
    EXPECT_THAT(names(*reloaded), ::testing::UnorderedElementsAre("kid", "parent"));

    const std::optional<Macro> parent = reloaded->macro("parent");
    ASSERT_TRUE(parent);
    ASSERT_EQ(parent->steps.size(), 1);
    EXPECT_EQ(std::get<MacroReference>(parent->steps.front()).name, "kid");
}

TEST_F(MacroManager_MacrosRepositoryTests, RemoveDeletesOnlyTheRemovedMacro)
{
    ASSERT_TRUE(m_repository->save(Macro { "my_macro", {} }));
    ASSERT_TRUE(m_repository->save(Macro { "my macro", {} }));

    ASSERT_TRUE(m_repository->remove("my macro"));

    EXPECT_THAT(names(*m_repository), ::testing::ElementsAre("my_macro"));
    EXPECT_THAT(names(*makeRepository()), ::testing::ElementsAre("my_macro"));
}

TEST_F(MacroManager_MacrosRepositoryTests, RemoveDropsReferencesFromOtherMacros)
{
    ASSERT_TRUE(m_repository->save(Macro { "child", {} }));
    ASSERT_TRUE(m_repository->save(Macro { "other", {} }));
    ASSERT_TRUE(m_repository->save(Macro { "parent",
                                           { MacroReference { "child" }, MacroReference { "other" }, MacroReference { "child" } } }));
    ASSERT_TRUE(m_repository->save(Macro { "grandparent", { MacroReference { "parent" } } }));

    EXPECT_THAT(m_repository->referencingMacros("child"), ::testing::ElementsAre("parent"));

    ASSERT_TRUE(m_repository->remove("child"));

    const std::shared_ptr<MacrosRepository> reloaded = makeRepository();
    EXPECT_THAT(names(*reloaded), ::testing::UnorderedElementsAre("other", "parent", "grandparent"));

    const std::optional<Macro> parent = reloaded->macro("parent");
    ASSERT_TRUE(parent);
    ASSERT_EQ(parent->steps.size(), 1);
    EXPECT_EQ(std::get<MacroReference>(parent->steps.front()).name, "other");

    const std::optional<Macro> grandparent = reloaded->macro("grandparent");
    ASSERT_TRUE(grandparent);
    EXPECT_EQ(grandparent->steps.size(), 1);
}

TEST_F(MacroManager_MacrosRepositoryTests, DependentMacrosFollowsReferencesTransitively)
{
    ASSERT_TRUE(m_repository->save(Macro { "child", {} }));
    ASSERT_TRUE(m_repository->save(Macro { "unrelated", {} }));
    ASSERT_TRUE(m_repository->save(Macro { "parent", { MacroReference { "child" } } }));
    ASSERT_TRUE(m_repository->save(Macro { "grandparent", { MacroReference { "parent" }, MacroReference { "child" } } }));

    EXPECT_THAT(m_repository->referencingMacros("child"), ::testing::UnorderedElementsAre("parent", "grandparent"));
    EXPECT_THAT(m_repository->dependentMacros("child"), ::testing::UnorderedElementsAre("parent", "grandparent"));
    EXPECT_THAT(m_repository->dependentMacros("parent"), ::testing::ElementsAre("grandparent"));
    EXPECT_THAT(m_repository->dependentMacros("unrelated"), ::testing::IsEmpty());
}
