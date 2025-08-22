#include "deletebehaviorpanelmodel.h"
#include "global/translation.h"

namespace au::trackedit {
DeleteBehaviorPanelModel::DeleteBehaviorPanelModel(QObject* parent)
    : QObject(parent)
{
}

int DeleteBehaviorPanelModel::deleteBehavior() const
{
    return static_cast<int>(m_deleteBehavior);
}

void DeleteBehaviorPanelModel::setDeleteBehavior(int value)
{
    const auto newBehavior = static_cast<DeleteBehavior>(value);
    if (m_deleteBehavior == newBehavior) {
        return;
    }
    m_deleteBehavior = newBehavior;
    emit deleteBehaviorChanged();
    emit userMustChooseCloseGapBehaviorChanged();
}

int DeleteBehaviorPanelModel::closeGapBehavior() const
{
    return static_cast<int>(m_closeGapBehavior);
}

void DeleteBehaviorPanelModel::setCloseGapBehavior(int value)
{
    const auto newBehavior = static_cast<CloseGapBehavior>(value);
    if (m_closeGapBehavior == newBehavior) {
        return;
    }
    m_closeGapBehavior = newBehavior;
    emit closeGapBehaviorChanged();
}

bool DeleteBehaviorPanelModel::userMustChooseCloseGapBehavior() const
{
    return m_deleteBehavior == DeleteBehavior::CloseGap;
}

QVariantList DeleteBehaviorPanelModel::closeGapBehaviors() const
{
    QVariantList behaviors;
    behaviors.append(QVariantMap { { "text", muse::qtrc("trackedit/preferences", "The selected clip moves back to fill the gap") },
                         { "value", static_cast<int>(CloseGapBehavior::ClipRipple) } });
    behaviors.append(QVariantMap { { "text", muse::qtrc("trackedit/preferences", "All clips on the same track move back to fill the gap") },
                         { "value", static_cast<int>(CloseGapBehavior::TrackRipple) } });
    behaviors.append(QVariantMap { { "text", muse::qtrc("trackedit/preferences", "All clips on all tracks move back to fill the gap") },
                         { "value", static_cast<int>(CloseGapBehavior::AllTracksRipple) } });
    return behaviors;
}

QVariantList DeleteBehaviorPanelModel::deleteBehaviors() const
{
    QVariantList behaviors;
    behaviors.append(QVariantMap { { "text", muse::qtrc("trackedit/preferences", "Leave gap") },
                         { "imageSource", "qrc:/resources/Leave_Gap.gif" },
                         { "value", static_cast<int>(DeleteBehavior::LeaveGap) } });
    behaviors.append(QVariantMap { { "text", muse::qtrc("trackedit/preferences", "Close gap (ripple)") },
                         { "imageSource", "qrc:/resources/Ripple.gif" },
                         { "value", static_cast<int>(DeleteBehavior::CloseGap) } });
    return behaviors;
}
}
