/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "context/tests/mocks/globalcontextmock.h"

#define private public

#include "trackedit/internal/au3/au3trackeditclipboard.h"

using namespace au::au3;
using namespace au::trackedit;

class au3interactionTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_globalContext = std::make_shared<au::context::GlobalContextMock>();

        m_clipboard = std::make_unique<Au3TrackeditClipboard>();

        m_clipboard->globalContext.set(m_globalContext);
    }

    std::unique_ptr<Au3TrackeditClipboard> m_clipboard;

    std::shared_ptr<au::context::GlobalContextMock> m_globalContext;
};

TEST_F(au3interactionTests, getGroupIDs)
{
    // TODO: I need to populate TrackData tuples.
    //  With mock data if possible.

    std::vector<TrackData> tracksData;

    auto result = m_clipboard->getGroupIDs(tracksData);

    // muse::Ret ret;
    // EXPECT_EQ(ret, make_ret(muse::Ret::Code::Ok));
}
