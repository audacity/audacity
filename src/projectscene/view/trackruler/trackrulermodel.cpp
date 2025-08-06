/*
* Audacity: A Digital Audio Editor
*/

#include "projectscene/view/trackruler/trackrulermodel.h"
#include "projectscene/view/trackruler/linearstereoruler.h"
#include "projectscene/view/trackruler/linearmonoruler.h"

using namespace au::projectscene;

TrackRulerModel::TrackRulerModel(QObject* parent)
    : QObject(parent)
{
    m_model = std::make_shared<LinearMonoRuler>();
}

std::vector<QVariantMap> TrackRulerModel::fullSteps() const
{
    if (!m_model) {
        return {};
    }

    const std::vector<TrackRulerFullStep>& steps = m_isCollapsed ? m_model->collapsedFullSteps() : m_model->fullSteps();
    std::vector<QVariantMap> variantSteps;
    std::transform(steps.begin(), steps.end(), std::back_inserter(variantSteps),
                   [&](const TrackRulerFullStep& step) {
        return QVariantMap {
            { "alignment", step.alignment },
            { "value", step.value },
            { "y", stepToPosition(step.value, step.channel) },
            { "channel", static_cast<int>(step.channel) },
            { "bold", step.isBold },
            { "fullWidthTick", step.fullWidthTick }
        };
    });

    return variantSteps;
}

std::vector<QVariantMap> TrackRulerModel::smallSteps() const
{
    if (!m_model) {
        return {};
    }

    const std::vector<TrackRulerSmallStep>& steps = m_isCollapsed ? m_model->collapsedSmallSteps() : m_model->smallSteps();
    std::vector<QVariantMap> variantSteps;
    std::transform(steps.begin(), steps.end(), std::back_inserter(variantSteps),
                   [&](const TrackRulerSmallStep& step) {
        return QVariantMap {
            { "channel", static_cast<int>(step.channel) },
            { "value", step.value },
            { "y", stepToPosition(step.value, step.channel) }
        };
    });
    return variantSteps;
}

bool TrackRulerModel::isStereo() const
{
    return m_isStereo;
}

void TrackRulerModel::setIsStereo(bool isStereo)
{
    if (m_isStereo != isStereo) {
        m_isStereo = isStereo;

        if (m_isStereo) {
            m_model = std::make_shared<LinearStereoRuler>();
        } else {
            m_model = std::make_shared<LinearMonoRuler>();
        }

        emit isStereoChanged();
        emit fullStepsChanged();
        emit smallStepsChanged();
        emit heightChanged();
    }
}

bool TrackRulerModel::isCollapsed() const
{
    return m_isCollapsed;
}

void TrackRulerModel::setIsCollapsed(bool isCollapsed)
{
    if (m_isCollapsed != isCollapsed) {
        m_isCollapsed = isCollapsed;
        emit isCollapsedChanged();
        emit fullStepsChanged();
        emit smallStepsChanged();
        emit heightChanged();
    }
}

int TrackRulerModel::height() const
{
    return m_height;
}

void TrackRulerModel::setHeight(int height)
{
    if (m_height != height) {
        m_height = height;
        emit heightChanged();
        emit fullStepsChanged();
        emit smallStepsChanged();
    }
}

double TrackRulerModel::stepToPosition(double step, int channel) const
{
    if (!m_model) {
        return 0.0;
    }

    return m_model->stepToPosition(step, channel, m_height);
}
