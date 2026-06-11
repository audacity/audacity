/*
* SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2025 Audacity Limited
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

// Regression test for: master-track realtime effects lost on save/reload.
//
// The master (project-wide) realtime-effect list is serialized in
// ProjectFileIO::WriteXML from a snapshot (SavedMasterEffectList), not the live
// list. Au3ProjectAccessor::load() takes that snapshot (empty) right after load;
// if the snapshot is not refreshed before SaveProject, master effects added since
// load are never written, and reopening loses them. This test reproduces the
// load -> add -> save -> reload round trip and asserts the effect survives.

#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "global/io/fileinfo.h"
#include "global/types/ret.h"

#include "project/internal/audacityproject.h"

#include "project/tests/mocks/trackeditprojectcreatormock.h"
#include "project/tests/mocks/projectviewstatecreatormock.h"
#include "trackedit/tests/mocks/clipboardmock.h"

#include "testtools.h"
#include "testing/testcontext.h"

// au3 realtime-effects (symbols come transitively from au3wrap's public link of
// au3-realtime-effects / au3-components).
#include "au3-project/Project.h"
#include "au3-realtime-effects/RealtimeEffectList.h"
#include "au3-realtime-effects/RealtimeEffectState.h"
#include "au3-components/EffectInterface.h"

namespace au::project {
namespace {
// Minimal EffectInstanceFactory so that RealtimeEffectList::AddState succeeds
// (it requires state->GetEffect() != nullptr). No real plugin / PluginManager is
// needed: serialization only writes the plugin id, and the state is retained on
// reload regardless. All methods return trivial/empty values; MakeInstance() is
// never called in this no-playback test.
class DummyEffectInstanceFactory final : public EffectInstanceFactory
{
public:
    // ComponentInterface
    PluginPath GetPath() const override { return {}; }
    ComponentInterfaceSymbol GetSymbol() const override { return {}; }
    VendorSymbol GetVendor() const override { return {}; }
    wxString GetVersion() const override { return {}; }
    TranslatableString GetDescription() const override { return {}; }

    // EffectDefinitionInterface
    EffectType GetType() const override { return EffectTypeProcess; }
    EffectFamilySymbol GetFamily() const override { return {}; }
    bool IsInteractive() const override { return false; }
    bool IsDefault() const override { return false; }
    RealtimeSince RealtimeSupport() const override { return RealtimeSince::Always; }
    bool SupportsAutomation() const override { return false; }

    // EffectSettingsManager
    bool SaveSettings(const EffectSettings&, CommandParameters&) const override { return true; }
    bool LoadSettings(const CommandParameters&, EffectSettings&) const override { return true; }
    RegistryPaths GetFactoryPresets() const override { return {}; }
    OptionalMessage LoadUserPreset(const RegistryPath&, EffectSettings&) const override { return {}; }
    bool SaveUserPreset(const RegistryPath&, const EffectSettings&) const override { return true; }
    OptionalMessage LoadFactoryPreset(int, EffectSettings&) const override { return {}; }
    OptionalMessage LoadFactoryDefaults(EffectSettings&) const override { return {}; }

    // EffectInstanceFactory
    std::shared_ptr<EffectInstance> MakeInstance() const override { return nullptr; }
};

const EffectInstanceFactory& dummyFactory()
{
    static DummyEffectInstanceFactory instance;
    return instance;
}
}

class Project_RealtimeEffectsPersistenceTests : public ::testing::Test
{
protected:
    muse::modularity::ContextPtr m_testCtx;
    std::unique_ptr<Audacity4Project> m_currentProject;
    std::shared_ptr<au::project::TrackeditProjectCreatorMock> m_trackeditProjectCreator;
    std::shared_ptr<au::projectscene::ProjectViewStateCreatorMock> m_projectViewStateCreator;
    std::shared_ptr<au::trackedit::ClipboardMock> m_clipboard;

    void SetUp() override
    {
        m_testCtx = au::testutils::makeTestContext();
        m_clipboard = std::make_shared<::testing::NiceMock<au::trackedit::ClipboardMock> >();
        m_currentProject = makeProject();
    }

    void TearDown() override
    {
        if (m_currentProject) {
            m_currentProject.reset();
        }
    }

    std::unique_ptr<Audacity4Project> makeProject()
    {
        auto project = std::make_unique<Audacity4Project>(m_testCtx);
        project->trackeditProjectCreator.set(m_trackeditProjectCreator);
        project->viewStateCreator.set(m_projectViewStateCreator);
        project->clipboard.set(m_clipboard);
        return project;
    }

    static ::AudacityProject& au3Project(const std::unique_ptr<Audacity4Project>& project)
    {
        return *reinterpret_cast<::AudacityProject*>(project->m_au3Project->au3ProjectPtr());
    }

    // Save through Au3ProjectAccessor::save() directly (the method under test),
    // bypassing the facade's thumbnail creation. Friendship is granted to this
    // fixture, not to the TEST_F-generated subclass, so the access must live here.
    static bool saveDirect(const std::unique_ptr<Audacity4Project>& project, const muse::io::path_t& path)
    {
        return project->m_au3Project->save(path);
    }
};

TEST_F(Project_RealtimeEffectsPersistenceTests, MasterEffectList_SurvivesSaveReload)
{
    const std::string srcPath
        = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup3").toStdString();
    const std::string dstPath
        = (muse::String::fromUtf8(au_project_tests_DATA_ROOT) + "/data/master_effect_roundtrip.aup3").toStdString();

    // Writable working copy of the (empty) project.
    testtools::removeIfExists(dstPath);
    ASSERT_TRUE(testtools::copyFile(srcPath, dstPath));

    const muse::io::path_t projectPath(dstPath);
    const PluginID kFakeId = wxString::FromUTF8("au-test:fake-master-effect");

    // Resolve the fake plugin id for the whole test, so AddState succeeds and the
    // state survives reload's SetID() -> GetEffect() re-resolution.
    RealtimeEffectState::EffectFactory::Scope effectFactoryScope {
        [](const PluginID&) -> const EffectInstanceFactory* { return &dummyFactory(); }
    };

    // 1) Load the project. Au3ProjectAccessor::load() takes the (empty) master snapshot.
    ASSERT_TRUE(m_currentProject->load(projectPath, false, "").success());

    // 2) Add a master realtime effect to the LIVE list (the snapshot is now stale).
    {
        auto& live = RealtimeEffectList::Get(au3Project(m_currentProject));
        ASSERT_EQ(live.GetStatesCount(), 0u);

        auto state = RealtimeEffectState::make_shared(kFakeId);
        ASSERT_TRUE(live.AddState(state));
        ASSERT_EQ(live.GetStatesCount(), 1u);
    }

    // 3) Save through the same path that had the bug (Au3ProjectAccessor::save ->
    //    SaveProject -> WriteXML), bypassing the facade's thumbnail creation.
    ASSERT_TRUE(saveDirect(m_currentProject, projectPath));

    // 4) Close and reload into a fresh project instance.
    m_currentProject->close();
    m_currentProject = makeProject();
    ASSERT_TRUE(m_currentProject->load(projectPath, false, "").success());

    // 5) The master effect must have round-tripped.
    auto& reloaded = RealtimeEffectList::Get(au3Project(m_currentProject));
    EXPECT_EQ(reloaded.GetStatesCount(), 1u);
    if (reloaded.GetStatesCount() == 1u) {
        EXPECT_EQ(reloaded.GetStateAt(0)->GetID().ToStdString(), kFakeId.ToStdString());
    }

    m_currentProject->close();

    testtools::removeIfExists(dstPath);
    testtools::removeIfExists(dstPath + "-wal");
    testtools::removeIfExists(dstPath + "-shm");
}
}
