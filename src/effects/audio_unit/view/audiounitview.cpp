/*
 * Audacity: A Digital Audio Editor
 */
#include "audiounitview.h"

#include <QQuickWindow>

#include "audiounitcontrol.h"

namespace au::effects {
AudioUnitView::AudioUnitView(QQuickItem* parent)
    : QQuickItem(parent)
{
}

AudioUnitView::~AudioUnitView()
{
    deinit();
}

int AudioUnitView::instanceId() const
{
    return m_instanceId;
}

void AudioUnitView::setInstanceId(int newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }
    m_instanceId = newInstanceId;
    emit instanceIdChanged();
}

void AudioUnitView::init()
{
    const auto instance = std::dynamic_pointer_cast<AudioUnitInstance>(instancesRegister()->instanceById(m_instanceId));

    // TODO: When design for this setting is ready, use the user preference here
    bool isGraphical = true;

    m_auControl = std::make_unique<AUControl>();
    if (!m_auControl->create(instance->GetComponent(),
                             instance->GetAudioUnit(), isGraphical)) {
        m_auControl.reset();
        return;
    }

    connect(m_auControl.get(), &AUControl::sizeChanged, this, [this](){
        updateViewGeometry();
    });

    embedNativeView();
    updateViewGeometry();
}

void AudioUnitView::deinit()
{
    m_auControl.reset();
}

void AudioUnitView::embedNativeView()
{
    if (!window() || !m_auControl) {
        return;
    }

    m_auControl->winId(); // Force creation of native window
    m_auControl->setParent(window());

    m_auControl->show();
}

void AudioUnitView::updateViewGeometry()
{
    if (!m_auControl) {
        return;
    }

    QSize requiredSize = m_auControl->size();
    const auto availableSize = window()->screen()->availableSize();

    const int titleBarHeight = window()->frameGeometry().height() - window()->geometry().height();

    const int requiredWidth = requiredSize.width();
    const int requiredHeight = requiredSize.height();

    const int availableWidth = availableSize.width() - 2 * m_sidePadding;
    const int availableHeight = availableSize.height() - titleBarHeight - m_topPadding - m_bottomPadding;

    const int newWidth = std::min(requiredWidth, availableWidth);
    const int newHeight = std::min(requiredHeight, availableHeight);

    const int implicitWidth = std::max(m_minimumWidth, newWidth);
    const int sidePadding = std::max(m_sidePadding, (implicitWidth - newWidth) / 2);

    setImplicitWidth(implicitWidth);
    setImplicitHeight(newHeight);

    m_auControl->setGeometry(sidePadding, m_topPadding, newWidth, newHeight);
}

int AudioUnitView::minimumWidth() const
{
    return m_minimumWidth;
}

void AudioUnitView::setMinimumWidth(int newMinimumWidth)
{
    if (m_minimumWidth == newMinimumWidth) {
        return;
    }
    m_minimumWidth = newMinimumWidth;
    emit minimumWidthChanged();
}

int AudioUnitView::bottomPadding() const
{
    return m_bottomPadding;
}

void AudioUnitView::setBottomPadding(int newBottomPadding)
{
    if (m_bottomPadding == newBottomPadding) {
        return;
    }
    m_bottomPadding = newBottomPadding;
    emit bottomPaddingChanged();
    updateViewGeometry();
}

int AudioUnitView::topPadding() const
{
    return m_topPadding;
}

void AudioUnitView::setTopPadding(int newTopPadding)
{
    if (m_topPadding == newTopPadding) {
        return;
    }
    m_topPadding = newTopPadding;
    emit topPaddingChanged();
    updateViewGeometry();
}

int AudioUnitView::sidePadding() const
{
    return m_sidePadding;
}

void AudioUnitView::setSidePadding(int newSidePadding)
{
    if (m_sidePadding == newSidePadding) {
        return;
    }
    m_sidePadding = newSidePadding;
    emit sidePaddingChanged();
    updateViewGeometry();
}
} // namespace au::effects
