/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <QFile>
#include <QTemporaryDir>

#include <vector>

#include "au3wrap/internal/au3project.h"
#include "au3-bus-track/BusTrack.h"
#include "au3-project/Project.h"
#include "au3-wave-track/WaveTrack.h"
#include "testing/testcontext.h"

namespace {
const wxString TEST_TRACK_BEFORE = "Before its bus";
const wxString TEST_MASTER_TRACK = "To the master";
const wxString TEST_GROUP = "Group";
const wxString TEST_SUBMIX = "Submix";
const wxString TEST_TRACK_AFTER = "After its bus";
constexpr float TEST_BUS_VOLUME = 0.5f;
constexpr float TEST_BUS_PAN = -0.25f;
}

namespace au::project {
class BusPersistenceTests : public ::testing::Test
{
protected:
    QTemporaryDir m_directory;

    QString projectPath() const { return m_directory.filePath("bus.aup4"); }

    static ::AudacityProject& project(au3::Au3ProjectAccessor& accessor)
    {
        return *reinterpret_cast<::AudacityProject*>(accessor.au3ProjectPtr());
    }

    static WaveTrack& addWaveTrack(::AudacityProject& project, const wxString& name)
    {
        auto& tracks = TrackList::Get(project);
        auto track = WaveTrackFactory::Get(project).Create(1);
        track->SetName(name);
        tracks.Add(track);
        return *track;
    }

    static BusTrack& addBus(::AudacityProject& project, const wxString& name)
    {
        auto& bus = *BusTrack::New(project);
        bus.SetName(name);
        return bus;
    }

    //! The name of where the track named `name` goes; empty for the master
    static wxString outputName(::AudacityProject& project, const wxString& name)
    {
        for (const auto track : TrackList::Get(project)) {
            if (track->GetName() == name) {
                const auto bus = TrackOutput::Get(*track).FindBus();
                return bus ? bus->GetName() : wxString {};
            }
        }
        ADD_FAILURE() << "No track " << name;
        return {};
    }

    void SetUp() override
    {
        ASSERT_TRUE(QFile::copy(QString::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup4", projectPath()));
    }
};

TEST_F(BusPersistenceTests, TracksGoWhereTheyWentWhenSavedAndReloaded)
{
    {
        //! [GIVEN] tracks going to busses listed before and after them, a bus going to another, and a track to the master
        au3::Au3ProjectAccessor accessor { au::testutils::makeTestContext() };
        ASSERT_TRUE(accessor.load(projectPath().toStdString(), false));
        auto& before = addWaveTrack(project(accessor), TEST_TRACK_BEFORE);
        addWaveTrack(project(accessor), TEST_MASTER_TRACK);
        auto& group = addBus(project(accessor), TEST_GROUP);
        auto& submix = addBus(project(accessor), TEST_SUBMIX);
        auto& after = addWaveTrack(project(accessor), TEST_TRACK_AFTER);
        TrackOutput::Get(before).SetBus(group.GetId());
        TrackOutput::Get(group).SetBus(submix.GetId());
        TrackOutput::Get(after).SetBus(submix.GetId());
        submix.SetVolume(TEST_BUS_VOLUME);
        submix.SetPan(TEST_BUS_PAN);

        //! [WHEN] the project is saved
        ASSERT_TRUE(accessor.save(projectPath().toStdString()));
        accessor.close();
    }

    //! [THEN] reloaded, they go to the same places, and the busses keep their volume and pan
    au3::Au3ProjectAccessor accessor { au::testutils::makeTestContext() };
    ASSERT_TRUE(accessor.load(projectPath().toStdString(), false));
    EXPECT_EQ(outputName(project(accessor), TEST_TRACK_BEFORE), TEST_GROUP);
    EXPECT_EQ(outputName(project(accessor), TEST_MASTER_TRACK), wxString {});
    EXPECT_EQ(outputName(project(accessor), TEST_GROUP), TEST_SUBMIX);
    EXPECT_EQ(outputName(project(accessor), TEST_SUBMIX), wxString {});
    EXPECT_EQ(outputName(project(accessor), TEST_TRACK_AFTER), TEST_SUBMIX);
    for (const auto bus : TrackList::Get(project(accessor)).Any<const BusTrack>()) {
        if (bus->GetName() == TEST_SUBMIX) {
            EXPECT_FLOAT_EQ(bus->GetVolume(), TEST_BUS_VOLUME);
            EXPECT_FLOAT_EQ(bus->GetPan(), TEST_BUS_PAN);
        }
    }
    accessor.close();
}

TEST_F(BusPersistenceTests, ABusCannotGoWhereItWouldFeedBackIntoItself)
{
    //! [GIVEN] a bus going to another
    au3::Au3ProjectAccessor accessor { au::testutils::makeTestContext() };
    ASSERT_TRUE(accessor.load(projectPath().toStdString(), false));
    auto& group = addBus(project(accessor), TEST_GROUP);
    auto& submix = addBus(project(accessor), TEST_SUBMIX);
    TrackOutput::Get(group).SetBus(submix.GetId());

    //! [THEN] the other can't go to it, nor can it go to itself; a track can go to either
    EXPECT_TRUE(TrackOutput::WouldFeedBack(submix, group));
    EXPECT_TRUE(TrackOutput::WouldFeedBack(group, group));
    EXPECT_FALSE(TrackOutput::WouldFeedBack(group, submix));
    auto& track = addWaveTrack(project(accessor), TEST_TRACK_BEFORE);
    EXPECT_FALSE(TrackOutput::WouldFeedBack(track, group));
    EXPECT_FALSE(TrackOutput::WouldFeedBack(track, submix));
    accessor.close();
}
}
