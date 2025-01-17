/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbarbpmitem.h"

using namespace au::playback;

PlaybackToolBarBPMItem::PlaybackToolBarBPMItem(const muse::ui::UiAction& action,
                                               muse::uicomponents::ToolBarItemType::Type type,
                                               QObject* parent)
    : muse::uicomponents::ToolBarItem(action, type, parent)
{
    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this](){
        onProjectChanged();
    });

    globalContext()->playbackState()->playbackStatusChanged().onReceive(this, [this](playback::PlaybackStatus status) {
        if (status == PlaybackStatus::Stopped) {
            setState(muse::ui::UiActionState::make_enabled());
        } else {
            setState(muse::ui::UiActionState::make_disabled());
        }
    });

    onProjectChanged();
}

double PlaybackToolBarBPMItem::currentValue() const
{
    return m_currentValue;
}

void PlaybackToolBarBPMItem::setCurrentValue(double value)
{
    if (qFuzzyCompare(m_currentValue, value)) {
        return;
    }

    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return;
    }

    trackedit::TimeSignature timeSignature = project->timeSignature();
    timeSignature.tempo = value;
    project->setTimeSignature(timeSignature);
}

void PlaybackToolBarBPMItem::onProjectChanged()
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return;
    }

    project->timeSignatureChanged().onReceive(this, [this](const trackedit::TimeSignature&) {
        updateValues();
    });

    updateValues();
}

void PlaybackToolBarBPMItem::updateValues()
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return;
    }

    trackedit::TimeSignature timeSignature = project->timeSignature();

    m_currentValue = timeSignature.tempo;
    emit currentValueChanged();
}
