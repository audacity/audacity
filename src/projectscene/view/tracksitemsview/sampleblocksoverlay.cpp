/*
* Audacity: A Digital Audio Editor
*/
#include "sampleblocksoverlay.h"

#include <QPainter>
#include <QRegion>

#include <cmath>

#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-wave-track/Sequence.h"
#include "au3-wave-track/SampleBlock.h"

#include "au3wrap/internal/domaccessor.h"

using namespace au::projectscene;

namespace {
constexpr qreal VISIBLE_PEN_WIDTH = 4.0;
constexpr qreal HIDDEN_PEN_WIDTH = 1.5;
constexpr qreal CHANNEL_MARGIN = 3.0;
constexpr qreal BLOCK_GAP = 2.0;
constexpr int HIDDEN_ALPHA = 150;

//! Same id -> same colour, so a block can be followed across edits.
QColor blockColor(long long blockId)
{
    if (blockId < 0) {
        // Silent blocks have negative ids
        return QColor(160, 160, 160);
    }
    const double goldenRatioConjugate = 0.618033988749895;
    const double hue = std::fmod(static_cast<double>(blockId) * goldenRatioConjugate, 1.0);
    return QColor::fromHsvF(hue, 0.85, 1.0);
}
}

SampleBlocksOverlay::SampleBlocksOverlay(QQuickItem* parent)
    : QQuickPaintedItem(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
    setAcceptedMouseButtons(Qt::NoButton);
    setAntialiasing(true);
}

void SampleBlocksOverlay::componentComplete()
{
    QQuickPaintedItem::componentComplete();

    configuration()->isSampleBlocksVisibleChanged().onReceive(this, [this](bool) {
        emit blocksVisibleChanged();
        update();
    }, muse::async::Asyncable::Mode::SetReplace);

    projectHistory()->historyChanged().onReceive(this, [this](auto) {
        update();
    }, muse::async::Asyncable::Mode::SetReplace);

    subscribeToProject();
    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]() {
        subscribeToProject();
        update();
    }, muse::async::Asyncable::Mode::SetReplace);
}

bool SampleBlocksOverlay::blocksVisible() const
{
    return configuration()->isSampleBlocksVisible();
}

void SampleBlocksOverlay::subscribeToProject()
{
    const auto prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }
    prj->trackChanged().onReceive(this, [this](const trackedit::Track&) {
        update();
    }, muse::async::Asyncable::Mode::SetReplace);
    prj->trackClipListChanged().onReceive(this, [this](const trackedit::Track&) {
        update();
    }, muse::async::Asyncable::Mode::SetReplace);
}

void SampleBlocksOverlay::paint(QPainter* painter)
{
    if (!blocksVisible() || !m_context) {
        return;
    }

    const auto project = globalContext()->currentProject();
    if (!project) {
        return;
    }
    auto* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*au3Project, ::TrackId(m_trackId));
    if (!track) {
        return;
    }

    const QRectF area = boundingRect();
    const size_t nChannels = track->NChannels();

    auto channelBand = [&](size_t ch) {
        if (nChannels < 2) {
            return QRectF(area.left(), area.top(), area.width(), area.height());
        }
        const qreal splitY = area.top() + area.height() * m_channelHeightRatio;
        return ch == 0
               ? QRectF(area.left(), area.top(), area.width(), splitY - area.top())
               : QRectF(area.left(), splitY, area.width(), area.bottom() - splitY);
    };

    QFont font = painter->font();
    font.setPixelSize(11);
    font.setBold(true);
    painter->setFont(font);
    const QFontMetricsF fm(font);

    for (const std::shared_ptr<WaveClip>& clip : track->Intervals()) {
        const double seqStartTime = clip->GetSequenceStartTime();
        const qreal playX0 = m_context->timeToPosition(clip->GetPlayStartTime());
        const qreal playX1 = m_context->timeToPosition(clip->GetPlayEndTime());

        const QRectF visibleSpan(playX0, area.top(), playX1 - playX0, area.height());
        const QRegion hiddenRegion = QRegion(area.toAlignedRect()) - QRegion(visibleSpan.toAlignedRect());

        for (size_t ch = 0; ch < clip->NChannels() && ch < nChannels; ++ch) {
            const BlockArray* blocks = clip->GetSequenceBlockArray(ch);
            if (!blocks) {
                continue;
            }
            const QRectF band = channelBand(ch).adjusted(0, CHANNEL_MARGIN, 0, -CHANNEL_MARGIN);

            for (const SeqBlock& block : *blocks) {
                if (!block.sb) {
                    continue;
                }
                const double t0 = seqStartTime + clip->SamplesToTime(block.start);
                const double t1 = seqStartTime + clip->SamplesToTime(block.start + block.sb->GetSampleCount());
                const qreal x0 = m_context->timeToPosition(t0);
                const qreal x1 = m_context->timeToPosition(t1);
                if (x1 < area.left() || x0 > area.right()) {
                    continue;
                }

                const long long id = block.sb->GetBlockID();
                const QColor color = blockColor(id);
                QColor hiddenColor = color;
                hiddenColor.setAlpha(HIDDEN_ALPHA);

                // Inset so neighbouring blocks' strokes don't merge
                const qreal inset = VISIBLE_PEN_WIDTH / 2 + BLOCK_GAP / 2;
                QRectF rect(x0, band.top(), x1 - x0, band.height());
                if (rect.width() > 2 * inset + 1) {
                    rect.adjust(inset, VISIBLE_PEN_WIDTH / 2, -inset, -VISIBLE_PEN_WIDTH / 2);
                }

                // Part of the block inside the clip's play region: fat solid stroke
                painter->save();
                painter->setClipRect(visibleSpan);
                painter->setPen(QPen(color, VISIBLE_PEN_WIDTH, Qt::SolidLine, Qt::SquareCap, Qt::MiterJoin));
                painter->setBrush(Qt::NoBrush);
                painter->drawRect(rect);
                painter->restore();

                // Part of the block in trimmed-away audio: thin dashed stroke
                painter->save();
                painter->setClipRegion(hiddenRegion);
                painter->setPen(QPen(hiddenColor, HIDDEN_PEN_WIDTH, Qt::DashLine));
                painter->setBrush(Qt::NoBrush);
                painter->drawRect(rect);
                painter->restore();

                // Id label in the upper-left corner, only if it fits in the block
                const QString label = QString::number(id);
                const qreal labelW = fm.horizontalAdvance(label) + 6;
                const qreal labelH = fm.height() + 2;
                if (rect.width() < labelW + VISIBLE_PEN_WIDTH || rect.height() < labelH + VISIBLE_PEN_WIDTH) {
                    continue;
                }
                const QRectF labelRect(rect.left() + VISIBLE_PEN_WIDTH / 2, rect.top() + VISIBLE_PEN_WIDTH / 2, labelW, labelH);
                const bool labelVisible = visibleSpan.contains(labelRect.topLeft());
                painter->save();
                painter->setPen(Qt::NoPen);
                painter->setBrush(labelVisible ? color : hiddenColor);
                painter->drawRect(labelRect);
                painter->setPen(QColor(0, 0, 0, labelVisible ? 255 : HIDDEN_ALPHA));
                painter->drawText(labelRect, Qt::AlignCenter, label);
                painter->restore();
            }
        }
    }
}

QVariant SampleBlocksOverlay::trackId() const
{
    return QVariant::fromValue(m_trackId);
}

void SampleBlocksOverlay::setTrackId(const QVariant& trackId)
{
    const trackedit::TrackId newTrackId = trackId.toInt();
    if (m_trackId == newTrackId) {
        return;
    }
    m_trackId = newTrackId;
    emit trackIdChanged();
    update();
}

TimelineContext* SampleBlocksOverlay::timelineContext() const
{
    return m_context;
}

void SampleBlocksOverlay::setTimelineContext(TimelineContext* context)
{
    if (m_context == context) {
        return;
    }
    if (m_context) {
        disconnect(m_context, nullptr, this, nullptr);
    }
    m_context = context;
    if (m_context) {
        connect(m_context, &TimelineContext::frameTimeChanged, this, [this]() { update(); });
        connect(m_context, &TimelineContext::zoomChanged, this, [this]() { update(); });
    }
    emit timelineContextChanged();
    update();
}

double SampleBlocksOverlay::channelHeightRatio() const
{
    return m_channelHeightRatio;
}

void SampleBlocksOverlay::setChannelHeightRatio(double ratio)
{
    if (qFuzzyCompare(m_channelHeightRatio, ratio)) {
        return;
    }
    m_channelHeightRatio = ratio;
    emit channelHeightRatioChanged();
    update();
}
