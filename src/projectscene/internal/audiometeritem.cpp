#include "audiometeritem.h"
#include "ui/uitypes.h"
#include <QtQuick/QSGNode>
#include <QtQuick/QSGSimpleRectNode>
#include <QtQuick/QSGGeometryNode>
#include <QtQuick/QSGVertexColorMaterial>
#include <QDateTime>
#include <qcolor.h>
#include <qopengl.h>

using namespace au::projectscene;

namespace {
constexpr float BACKGROUND_ALPHA = 0.7f;

constexpr const char* CLIPPED_COLOR = "#EF476F";
constexpr const char* PEAK_COLOR = "#14151A";
constexpr const char* RMS_OVERLAY_COLOR = "#66000000";

constexpr const char* GRADIENT_GREEN_COLOR = "#50DF46";
constexpr const char* GRADIENT_YELLOW_COLOR = "#FFE100";
constexpr const char* GRADIENT_RED_COLOR = "#EF476F";
}

AudioMeterItem::AudioMeterItem(QQuickItem* parent)
    : QQuickItem(parent)
{
    setFlag(ItemHasContents, true);
}

void AudioMeterItem::load()
{
    uiConfiguration()->currentThemeChanged().onNotify(this, [this] {
        m_fullRedrawNeeded = true;
        m_accentColor = uiConfiguration()->currentTheme().values.value(muse::ui::ACCENT_COLOR).value<QColor>();
        m_strokeColor = uiConfiguration()->currentTheme().values.value(muse::ui::STROKE_COLOR).value<QColor>();
        m_btnColor = uiConfiguration()->currentTheme().values.value(muse::ui::BUTTON_COLOR).value<QColor>();
        m_clippedColor = QColor(CLIPPED_COLOR);
        update();
    });

    connect(m_meterModel, &au::playback::PlaybackMeterModel::meterStyleChanged, this, [this] {
        m_fullRedrawNeeded = true;
        update();
    });

    connect(this, &QQuickItem::heightChanged, this, [this] { m_fullRedrawNeeded = true; update(); });
    connect(this, &QQuickItem::widthChanged, this, [this] { m_fullRedrawNeeded = true; update(); });

    m_accentColor = uiConfiguration()->currentTheme().values.value(muse::ui::ACCENT_COLOR).value<QColor>();
    m_strokeColor = uiConfiguration()->currentTheme().values.value(muse::ui::STROKE_COLOR).value<QColor>();
    m_btnColor = uiConfiguration()->currentTheme().values.value(muse::ui::BUTTON_COLOR).value<QColor>();
    m_clippedColor = QColor(CLIPPED_COLOR);
}

QSGNode* AudioMeterItem::createNodes()
{
    auto* rootNode = new QSGNode();

    QColor bgColor = m_strokeColor;
    bgColor.setAlphaF(BACKGROUND_ALPHA);
    m_backgroundNode = new QSGSimpleRectNode(QRectF(0, 0, width(), height()), bgColor);
    rootNode->appendChildNode(m_backgroundNode);
    m_backgroundNode->setFlag(QSGNode::OwnedByParent);

    m_volumeBarNode = new QSGSimpleRectNode();
    rootNode->appendChildNode(m_volumeBarNode);
    m_volumeBarNode->setFlag(QSGNode::OwnedByParent);

    m_clippedNode = new QSGSimpleRectNode();
    rootNode->appendChildNode(m_clippedNode);
    m_clippedNode->setFlag(QSGNode::OwnedByParent);

    m_recentPeakNode = new QSGSimpleRectNode(QRectF(0, 0, 0, 0), m_accentColor);
    rootNode->appendChildNode(m_recentPeakNode);
    m_recentPeakNode->setFlag(QSGNode::OwnedByParent);

    m_maxPeakNode = new QSGSimpleRectNode(QRectF(0, 0, 0, 0), QColor(PEAK_COLOR));
    rootNode->appendChildNode(m_maxPeakNode);
    m_maxPeakNode->setFlag(QSGNode::OwnedByParent);

    m_rmsNode = new QSGSimpleRectNode(QRectF(0, 0, 0, 0), QColor(RMS_OVERLAY_COLOR));
    rootNode->appendChildNode(m_rmsNode);
    m_rmsNode->setFlag(QSGNode::OwnedByParent);

    m_gradientNode = new QSGGeometryNode();
    auto* geometry = new QSGGeometry(QSGGeometry::defaultAttributes_ColoredPoint2D(), 6);
    geometry->setDrawingMode(GL_TRIANGLE_STRIP);
    m_gradientNode->setGeometry(geometry);
    m_gradientNode->setFlag(QSGNode::OwnsGeometry);
    auto* material = new QSGVertexColorMaterial;
    m_gradientNode->setMaterial(material);
    m_gradientNode->setFlag(QSGNode::OwnsMaterial);
    rootNode->appendChildNode(m_gradientNode);

    return rootNode;
}

void AudioMeterItem::reset()
{
    m_maxPeak = MIN_DB;
    m_lastMaxPeak = MIN_DB;
    m_recentPeak = MIN_DB;
    m_lastRecentPeak = MIN_DB;
    m_currentVolumePressure = MIN_DB;
    m_lastVolumePressure = MIN_DB;
    m_currentRMS = MIN_DB;
    m_lastRMS = MIN_DB;

    m_recentVolumePressure.clear();
    update();
}

void AudioMeterItem::resetClipped()
{
    if (m_clipped) {
        m_clipped = false;
        update();
    }
}

void AudioMeterItem::updatePeaks()
{
    const int64_t now = QDateTime::currentMSecsSinceEpoch();
    m_recentVolumePressure.push_back({ m_currentVolumePressure, now });

    const int64_t cutoffTime = now - m_recentPeakIntervalMsecs;
    auto iter = std::remove_if(m_recentVolumePressure.begin(), m_recentVolumePressure.end(),
                               [cutoffTime](const VolumeSample& sample){ return sample.timestamp < cutoffTime; });
    m_recentVolumePressure.erase(iter, m_recentVolumePressure.end());

    auto max_value_it = std::max_element(m_recentVolumePressure.begin(), m_recentVolumePressure.end(),
                                         [](const VolumeSample& a, const VolumeSample& b) {
        return a.value < b.value;
    });

    m_recentPeak = (max_value_it != m_recentVolumePressure.end()) ? max_value_it->value : MIN_DB;
    m_maxPeak = std::max(m_maxPeak, m_currentVolumePressure);
}

float AudioMeterItem::sampleToY(float sampleValue) const
{
    if (m_meterModel == nullptr) {
        return 0;
    }

    return static_cast<float>((height() - m_overloadHeight) * m_meterModel->sampleToPosition(sampleValue));
}

QSGNode* AudioMeterItem::updatePaintNode(QSGNode* oldNode, UpdatePaintNodeData*)
{
    QSGNode* rootNode = oldNode;
    if (rootNode == nullptr) {
        rootNode = createNodes();
    }

    drawBackground();
    drawVolumeBar();
    drawClippedIndicator();
    drawRecentPeakIndicator();
    drawMaxPeakIndicator();

    m_fullRedrawNeeded = false;

    return rootNode;
}

void AudioMeterItem::drawBackground()
{
    if (m_backgroundNode == nullptr) {
        return;
    }

    if (!m_fullRedrawNeeded) {
        return;
    }

    QColor bgColor = m_strokeColor;
    static constexpr auto BACKGROUND_ALPHA = 0.7;
    bgColor.setAlphaF(BACKGROUND_ALPHA);
    m_backgroundNode->setColor(bgColor);
    m_backgroundNode->setRect(0, 0, width(), height());
}

void AudioMeterItem::drawVolumeBar()
{
    if (m_meterModel == nullptr) {
        return;
    }

    if (m_fullRedrawNeeded) {
        m_rmsNode->setRect(0, 0, 0, 0);
        m_volumeBarNode->setRect(0, 0, 0, 0);

        auto* geometry = m_gradientNode->geometry();
        auto* vertices = geometry->vertexDataAsColoredPoint2D();
        for (int i = 0; i < 6; ++i) {
            vertices[i].set(0, 0, m_accentColor.red(), m_accentColor.green(), m_accentColor.blue(), m_accentColor.alpha());
        }
        geometry->markVertexDataDirty();
        m_gradientNode->markDirty(QSGNode::DirtyGeometry);
    }

    if (m_meterModel->meterStyle() == playback::PlaybackMeterStyle::MeterStyle::Default) {
        drawVolumeBarDefault();
    } else if (m_meterModel->meterStyle() == playback::PlaybackMeterStyle::MeterStyle::RMS) {
        drawVolumeBarRMS();
    } else if (m_meterModel->meterStyle() == playback::PlaybackMeterStyle::MeterStyle::Gradient) {
        drawVolumeBarGradient();
    }
}

void AudioMeterItem::drawVolumeBarDefault()
{
    if (m_volumeBarNode == nullptr) {
        return;
    }

    if (!m_fullRedrawNeeded && qFuzzyCompare(m_lastVolumePressure, m_currentVolumePressure)) {
        return;
    }

    if (m_currentVolumePressure >= 0) {
        m_volumeBarNode->setColor(m_clippedColor);
        m_volumeBarNode->setRect(0, 0, width(), height());
    } else {
        m_volumeBarNode->setColor(m_accentColor);
        const auto sampleY = sampleToY(m_currentVolumePressure);
        m_volumeBarNode->setRect(0, height() - sampleY, width(), sampleY);
    }

    m_lastVolumePressure = m_currentVolumePressure;
    m_lastRMS = m_currentRMS;
}

void AudioMeterItem::drawVolumeBarRMS()
{
    if (m_volumeBarNode == nullptr || m_rmsNode == nullptr) {
        return;
    }

    if (!m_fullRedrawNeeded && qFuzzyCompare(m_lastRMS, m_currentRMS)) {
        return;
    }

    if (m_currentVolumePressure >= 0) {
        m_volumeBarNode->setColor(m_clippedColor);
        m_volumeBarNode->setRect(0, 0, width(), height());
        m_rmsNode->setRect(0, 0, 0, 0);
    } else {
        m_volumeBarNode->setColor(m_accentColor);
        const auto sampleY = sampleToY(m_currentVolumePressure);
        m_volumeBarNode->setRect(0, height() - sampleY, width(), sampleY);
        const auto rmsY = sampleToY(m_currentRMS);
        m_rmsNode->setRect(0, height() - sampleY, width(), sampleY - rmsY);
    }

    m_lastVolumePressure = m_currentVolumePressure;
    m_lastRMS = m_currentRMS;
}

void AudioMeterItem::drawVolumeBarGradient()
{
    if (m_gradientNode == nullptr) {
        return;
    }

    if (!m_fullRedrawNeeded && qFuzzyCompare(m_lastVolumePressure, m_currentVolumePressure)) {
        return;
    }

    const QColor redColor = QColor(GRADIENT_RED_COLOR);
    const QColor yellowColor = QColor(GRADIENT_YELLOW_COLOR);
    const QColor greenColor = QColor(GRADIENT_GREEN_COLOR);

    auto* geometry = m_gradientNode->geometry();
    auto* vertices = geometry->vertexDataAsColoredPoint2D();

    const float yellowStop = 0.8f;
    const float yellowSectionHeight = height() * yellowStop;

    const float currentBarHeight = sampleToY(m_currentVolumePressure);
    const float rectWidth = width();

    float middleY, topY;
    QColor middleColor, topColor;

    if (currentBarHeight <= yellowSectionHeight) {
        // Bar is short: Top of the bar is in the green->yellow zone.
        middleY = height() - currentBarHeight;
        const float localPercent = (currentBarHeight > 0) ? (currentBarHeight / yellowSectionHeight) : 0;
        middleColor = QColor::fromRgbF(
            (1.0f - localPercent) * greenColor.redF() + localPercent * yellowColor.redF(),
            (1.0f - localPercent) * greenColor.greenF() + localPercent * yellowColor.greenF(),
            (1.0f - localPercent) * greenColor.blueF() + localPercent * yellowColor.blueF()
            );

        // Collapse the top quad by setting its vertices equal to the middle ones.
        topY = middleY;
        topColor = middleColor;
    } else {
        // Bar is tall: Top of the bar is in the yellow->red zone.
        middleY = height() - yellowSectionHeight;
        middleColor = yellowColor;

        topY = height() - currentBarHeight;
        const float topSectionHeight = currentBarHeight - yellowSectionHeight;
        const float redSectionTotalHeight = height() - yellowSectionHeight;
        const float localPercent = topSectionHeight / redSectionTotalHeight;
        topColor = QColor::fromRgbF(
            (1.0f - localPercent) * yellowColor.redF() + localPercent * redColor.redF(),
            (1.0f - localPercent) * yellowColor.greenF() + localPercent * redColor.greenF(),
            (1.0f - localPercent) * yellowColor.blueF() + localPercent * redColor.blueF()
            );
    }

    vertices[0].set(0, height(), greenColor.red(), greenColor.green(), greenColor.blue(), 255);
    vertices[1].set(rectWidth, height(), greenColor.red(), greenColor.green(), greenColor.blue(), 255);
    vertices[2].set(0, middleY, middleColor.red(), middleColor.green(), middleColor.blue(), 255);
    vertices[3].set(rectWidth, middleY, middleColor.red(), middleColor.green(), middleColor.blue(), 255);
    vertices[4].set(0, topY, topColor.red(), topColor.green(), topColor.blue(), 255);
    vertices[5].set(rectWidth, topY, topColor.red(), topColor.green(), topColor.blue(), 255);
    geometry->markVertexDataDirty();
    m_gradientNode->markDirty(QSGNode::DirtyGeometry);

    m_lastVolumePressure = m_currentVolumePressure;
    m_lastRMS = m_currentRMS;
}

void AudioMeterItem::drawClippedIndicator()
{
    if (m_clippedNode == nullptr) {
        return;
    }

    if (!m_fullRedrawNeeded && (m_lastClipped == m_clipped)) {
        return;
    }

    if (m_showClippedInfo) {
        m_clippedNode->setColor(m_clipped ? m_clippedColor : m_btnColor);
        m_clippedNode->setRect(0, 0, width(), m_overloadHeight);
    } else {
        m_clippedNode->setRect(0, 0, 0, 0);
    }

    m_lastClipped = m_clipped;
}

void AudioMeterItem::drawRecentPeakIndicator()
{
    if (m_recentPeakNode == nullptr) {
        return;
    }

    if (!m_fullRedrawNeeded && qFuzzyCompare(m_lastRecentPeak, m_recentPeak)) {
        return;
    }

    m_recentPeakNode->setRect(0, height() - sampleToY(m_recentPeak), width(), 1);
    m_lastRecentPeak = m_recentPeak;
}

void AudioMeterItem::drawMaxPeakIndicator()
{
    if (m_maxPeakNode == nullptr) {
        return;
    }

    if (!m_fullRedrawNeeded && qFuzzyCompare(m_lastMaxPeak, m_maxPeak)) {
        return;
    }

    const auto maxPeak = sampleToY(m_maxPeak);
    if (maxPeak > 0) {
        m_maxPeakNode->setRect(0, height() - maxPeak, width(), 1);
    } else {
        m_maxPeakNode->setRect(0, 0, 0, 0);
    }

    m_lastMaxPeak = m_maxPeak;
}

float AudioMeterItem::currentVolumePressure() const
{
    return m_currentVolumePressure;
}

void AudioMeterItem::setCurrentVolumePressure(float value)
{
    if (qFuzzyCompare(m_currentVolumePressure, value)) {
        return;
    }

    m_currentVolumePressure = value;
    updatePeaks();

    if (m_currentVolumePressure >= 0.0) {
        if (!m_clipped) {
            m_clipped = true;
        }
    }
    update();
}

float AudioMeterItem::currentRMS() const
{
    return m_currentRMS;
}

void AudioMeterItem::setCurrentRMS(float value)
{
    if (qFuzzyCompare(m_currentRMS, value)) {
        return;
    }

    m_currentRMS = value;
    update();
}

bool AudioMeterItem::showClippedInfo() const
{
    return m_showClippedInfo;
}

void AudioMeterItem::setShowClippedInfo(bool show)
{
    if (m_showClippedInfo == show) {
        return;
    }

    m_showClippedInfo = show;
    update();
}

int AudioMeterItem::recentPeakInterval() const
{
    return m_recentPeakIntervalMsecs;
}

void AudioMeterItem::setRecentPeakInterval(int intervalMsecs)
{
    if (m_recentPeakIntervalMsecs == intervalMsecs) {
        return;
    }

    m_recentPeakIntervalMsecs = intervalMsecs;
    update();
}

int AudioMeterItem::overloadHeight() const
{
    return m_overloadHeight;
}

void AudioMeterItem::setOverloadHeight(int height)
{
    if (m_overloadHeight == height) {
        return;
    }

    m_overloadHeight = height;
    update();
}

au::playback::PlaybackMeterModel* AudioMeterItem::meterModel() const
{
    return m_meterModel;
}

void AudioMeterItem::setMeterModel(au::playback::PlaybackMeterModel* model)
{
    if (m_meterModel == model) {
        return;
    }

    m_meterModel = model;
    update();
}
