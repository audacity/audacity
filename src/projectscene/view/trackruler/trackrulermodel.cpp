/*
* Audacity: A Digital Audio Editor
*/

#include "trackrulermodel.h"

#include "internal/projectviewstate.h"
#include "trackedit/dom/track.h"
#include "projectscene/view/trackruler/linearstereoruler.h"
#include "projectscene/view/trackruler/linearmonoruler.h"
#include "projectscene/view/trackruler/dblogmonoruler.h"
#include "projectscene/view/trackruler/dblogstereoruler.h"
#include "projectscene/view/trackruler/dblinearmonoruler.h"
#include "projectscene/view/trackruler/dblinearstereoruler.h"

using namespace au::projectscene;

TrackRulerModel::TrackRulerModel(QObject* parent)
    : QObject(parent)
{
    m_model = buildRulerModel();
}

IProjectViewStatePtr TrackRulerModel::viewState() const
{
    au::project::IAudacityProjectPtr prj = globalContext()->currentProject();
    return prj ? prj->viewState() : nullptr;
}

void TrackRulerModel::init()
{
    m_model->setDbRange(au::playback::PlaybackMeterDbRange::toDouble(configuration()->playbackMeterDbRange()));
    configuration()->playbackMeterDbRangeChanged().onNotify(this, [this]() {
        m_model->setDbRange(au::playback::PlaybackMeterDbRange::toDouble(configuration()->playbackMeterDbRange()));
        emit fullStepsChanged();
        emit smallStepsChanged();
    });

    const auto prjViewState = viewState();
    if (prjViewState == nullptr) {
        return;
    }

    prjViewState->trackRulerType(m_trackId).ch.onReceive(this, [this](int) {
        m_model = buildRulerModel();
        emit fullStepsChanged();
        emit smallStepsChanged();
    }, muse::async::Asyncable::Mode::SetReplace);

    prjViewState->verticalDisplayBounds(m_trackId).ch.onReceive(this, [this](std::pair<float, float> bounds) {
        m_model->setDisplayBounds(bounds);
        emit fullStepsChanged();
        emit smallStepsChanged();
    }, muse::async::Asyncable::Mode::SetReplace);
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

int TrackRulerModel::rulerType() const
{
    const auto prjViewState = viewState();
    if (prjViewState == nullptr) {
        return static_cast<int>(au::trackedit::TrackRulerType::Linear);
    }

    return prjViewState->trackRulerType(m_trackId).val;
}

void TrackRulerModel::setRulerType(int rulerType)
{
    const auto prjViewState = viewState();
    if (prjViewState == nullptr) {
        return;
    }

    prjViewState->setTrackRulerType(m_trackId, rulerType);

    m_model = buildRulerModel();

    emit rulerTypeChanged();
    emit fullStepsChanged();
    emit smallStepsChanged();
    emit isDefaultZoomChanged();
    emit isMaxZoomChanged();
    emit isMinZoomChanged();
}

QVariant TrackRulerModel::availableRulerTypes() const
{
    return QVariant::fromValue(QList<QMap<QString, QVariant> > {
        { { "label", "Logarithmic (dB)" }, { "value", static_cast<int>(au::trackedit::TrackRulerType::DbLog) } },
        { { "label", "Linear (dB)" }, { "value", static_cast<int>(au::trackedit::TrackRulerType::DbLinear) } },
        { { "label", "Linear (amp)" }, { "value", static_cast<int>(au::trackedit::TrackRulerType::Linear) } },
    });
}

bool TrackRulerModel::isRulerTypeLinear() const
{
    return rulerType() != static_cast<int>(au::trackedit::TrackRulerType::DbLog);
}

QVariant TrackRulerModel::displayBounds() const
{
    const auto prjViewState = viewState();
    if (prjViewState == nullptr) {
        return QVariant();
    }

    muse::ValCh<std::pair<float, float> > bounds = prjViewState->verticalDisplayBounds(m_trackId);
    QVariant::fromValue(QMap<QString, QVariant> {
        { "min", bounds.val.first },
        { "max", bounds.val.second }
    });
}

std::shared_ptr<ITrackRuler> TrackRulerModel::buildRulerModel()
{
    std::shared_ptr<ITrackRuler> model = nullptr;

    const auto rulerType = static_cast<trackedit::TrackRulerType>(this->rulerType());
    switch (rulerType) {
    case trackedit::TrackRulerType::DbLog:
        if (m_isStereo) {
            model = std::make_shared<DbLogStereoRuler>();
        } else {
            model = std::make_shared<DbLogMonoRuler>();
        }
        break;
    case trackedit::TrackRulerType::DbLinear:
        if (m_isStereo) {
            model = std::make_shared<DbLinearStereoRuler>();
        } else {
            model = std::make_shared<DbLinearMonoRuler>();
        }
        break;
    case trackedit::TrackRulerType::Linear:
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

    const auto prjViewState = viewState();
    if (prjViewState != nullptr) {
        std::pair<float, float> bounds = prjViewState->verticalDisplayBounds(m_trackId).val;
        model->setDisplayBounds(bounds);
    }

    return model;
}

void TrackRulerModel::zoomIn()
{
    const auto prjViewState = viewState();
    if (!prjViewState) {
        return;
    }

    prjViewState->zoomInVertically(m_trackId);

    emit isDefaultZoomChanged();
    emit isMaxZoomChanged();
    emit isMinZoomChanged();
    emit fullStepsChanged();
    emit smallStepsChanged();
}

void TrackRulerModel::zoomOut()
{
    const auto prjViewState = viewState();
    if (!prjViewState) {
        return;
    }

    prjViewState->zoomOutVertically(m_trackId);

    emit isDefaultZoomChanged();
    emit isMaxZoomChanged();
    emit isMinZoomChanged();
    emit fullStepsChanged();
    emit smallStepsChanged();
}

void TrackRulerModel::resetZoom()
{
    const auto prjViewState = viewState();
    if (!prjViewState) {
        return;
    }

    prjViewState->resetVerticalZoom(m_trackId);

    emit isDefaultZoomChanged();
    emit isMaxZoomChanged();
    emit isMinZoomChanged();
    emit fullStepsChanged();
    emit smallStepsChanged();
}

void TrackRulerModel::toggleHalfWave()
{
    const auto prjViewState = viewState();
    if (!prjViewState) {
        return;
    }

    prjViewState->toggleHalfWave(m_trackId);

    emit isHalfWaveChanged();
}

bool TrackRulerModel::isDefaultZoom() const
{
    const auto prjViewState = viewState();
    if (!prjViewState) {
        return false;
    }

    return prjViewState->isDefaultVerticalZoom(m_trackId);
}

bool TrackRulerModel::isMaxZoom() const
{
    const auto prjViewState = viewState();
    if (!prjViewState) {
        return false;
    }

    return prjViewState->isMaxVerticalZoom(m_trackId);
}

bool TrackRulerModel::isMinZoom() const
{
    const auto prjViewState = viewState();
    if (!prjViewState) {
        return false;
    }

    return prjViewState->isMinVerticalZoom(m_trackId);
}

bool TrackRulerModel::isHalfWave() const
{
    const auto prjViewState = viewState();
    if (!prjViewState) {
        return false;
    }

    return prjViewState->isHalfWave(m_trackId).val;
}

void TrackRulerModel::setTrackId(const trackedit::TrackId& newTrackId)
{
    if (m_trackId != newTrackId) {
        m_trackId = newTrackId;
        emit trackIdChanged();
    }
}

au::trackedit::TrackId TrackRulerModel::trackId() const
{
    return m_trackId;
}
