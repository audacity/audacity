/*
* Audacity: A Digital Audio Editor
*/

#include "trackrulermodel.h"
#include "linearstereoruler.h"
#include "linearmonoruler.h"
#include "dblogmonoruler.h"

using namespace au::projectscene;

TrackRulerModel::TrackRulerModel(QObject* parent)
    : QObject(parent)
{
    m_model = buildRulerModel();
}

void TrackRulerModel::init()
{
    m_model->setDbRange(au::playback::PlaybackMeterDbRange::toDouble(configuration()->playbackMeterDbRange()));
    configuration()->playbackMeterDbRangeChanged().onNotify(this, [this]() {
        m_model->setDbRange(au::playback::PlaybackMeterDbRange::toDouble(configuration()->playbackMeterDbRange()));
        emit fullStepsChanged();
        emit smallStepsChanged();
    });
}

std::vector<QVariantMap> TrackRulerModel::fullSteps() const
{
    if (!m_model) {
        return {};
    }

    const std::vector<TrackRulerFullStep>& steps = m_model->fullSteps();
    std::vector<QVariantMap> variantSteps;
    std::transform(steps.begin(), steps.end(), std::back_inserter(variantSteps),
                   [&](const TrackRulerFullStep& step) {
        return QVariantMap {
            { "alignment", step.alignment },
            { "value", step.value },
            { "y", stepToPosition(step.value, step.channel, step.isNegativeSample) },
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

    const std::vector<TrackRulerSmallStep>& steps = m_model->smallSteps();
    std::vector<QVariantMap> variantSteps;
    std::transform(steps.begin(), steps.end(), std::back_inserter(variantSteps),
                   [&](const TrackRulerSmallStep& step) {
        return QVariantMap {
            { "channel", static_cast<int>(step.channel) },
            { "value", step.value },
            { "y", stepToPosition(step.value, step.channel, step.isNegativeSample) }
        };
    });
    return variantSteps;
}

QString TrackRulerModel::sampleToText(double sample) const
{
    if (!m_model) {
        return QString();
    }

    return QString::fromStdString(m_model->sampleToText(sample));
}

bool TrackRulerModel::isStereo() const
{
    return m_isStereo;
}

void TrackRulerModel::setIsStereo(bool isStereo)
{
    if (m_isStereo != isStereo) {
        m_isStereo = isStereo;

        m_model = buildRulerModel();
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
        m_model->setCollapsed(isCollapsed);
        emit fullStepsChanged();
        emit smallStepsChanged();
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
        m_model->setHeight(height);
        emit fullStepsChanged();
        emit smallStepsChanged();
    }
}

double TrackRulerModel::stepToPosition(double step, int channel, bool isNegativeSample) const
{
    if (!m_model) {
        return 0.0;
    }

    return m_model->stepToPosition(step, channel, isNegativeSample);
}

double TrackRulerModel::channelHeightRatio() const
{
    return m_channelHeightRatio;
}

void TrackRulerModel::setChannelHeightRatio(double channelHeightRatio)
{
    if (m_channelHeightRatio != channelHeightRatio) {
        m_channelHeightRatio = channelHeightRatio;
        m_model->setChannelHeightRatio(channelHeightRatio);
        emit fullStepsChanged();
        emit smallStepsChanged();
    }
}

int TrackRulerModel::trackId() const
{
    return m_trackId;
}

void TrackRulerModel::setTrackId(int trackId)
{
    if (m_trackId == trackId) {
        return;
    }

    m_trackId = trackId;

    emit trackIdChanged();
    emit fullStepsChanged();
    emit smallStepsChanged();
}

int TrackRulerModel::rulerType() const
{
    return m_rulerType;
}

void TrackRulerModel::setRulerType(int rulerType)
{
    if (m_rulerType == rulerType) {
        return;
    }

    m_rulerType = rulerType;

    m_model = buildRulerModel();

    emit rulerTypeChanged();
    emit fullStepsChanged();
    emit smallStepsChanged();
}

std::shared_ptr<ITrackRulerModel> TrackRulerModel::buildRulerModel()
{
    std::shared_ptr<ITrackRulerModel> model = nullptr;

    switch (m_rulerType) {
    case 0:
        if (m_isStereo) {
            //model = std::make_shared<DbLogStereoRuler>();
        } else {
            model = std::make_shared<DbLogMonoRuler>();
        }
        break;
    case 1:
        if (m_isStereo) {
            //model = std::make_shared<DbLinearStereoRuler>();
        } else {
            //model = std::make_shared<DbLinearMonoRuler>();
        }
        break;
    case 2:
        if (m_isStereo) {
            model = std::make_shared<LinearStereoRuler>();
        } else {
            model = std::make_shared<LinearMonoRuler>();
        }
        break;
    default:
        model = std::make_shared<LinearMonoRuler>();
        break;
    }

    model->setHeight(m_height);
    model->setChannelHeightRatio(m_channelHeightRatio);
    model->setCollapsed(m_isCollapsed);
    model->setDbRange(au::playback::PlaybackMeterDbRange::toDouble(configuration()->playbackMeterDbRange()));

    return model;
}
