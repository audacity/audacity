/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbartimesignatureitem.h"

using namespace au::playback;

PlaybackToolBarTimeSignatureItem::PlaybackToolBarTimeSignatureItem(const muse::ui::UiAction& action,
                                                                   muse::uicomponents::ToolBarItemType::Type type,
                                                                   QObject* parent)
    : muse::uicomponents::ToolBarItem(action, type, parent)
{
    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this](){
        onProjectChanged();
    });

    onProjectChanged();
}

int PlaybackToolBarTimeSignatureItem::upper() const
{
    return m_upper;
}

void PlaybackToolBarTimeSignatureItem::setUpper(int newUpper)
{
    if (m_upper == newUpper) {
        return;
    }

    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return;
    }

    trackedit::TimeSignature timeSignature = project->timeSignature();
    timeSignature.upper = newUpper;
    project->setTimeSignature(timeSignature);
}

int PlaybackToolBarTimeSignatureItem::lower() const
{
    return m_lower;
}

void PlaybackToolBarTimeSignatureItem::setLower(int newLower)
{
    if (m_lower == newLower) {
        return;
    }

    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return;
    }

    trackedit::TimeSignature timeSignature = project->timeSignature();
    timeSignature.lower = newLower;
    project->setTimeSignature(timeSignature);
}

void PlaybackToolBarTimeSignatureItem::onProjectChanged()
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

void PlaybackToolBarTimeSignatureItem::updateValues()
{
    auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return;
    }

    trackedit::TimeSignature timeSignature = project->timeSignature();

    m_upper = timeSignature.upper;
    emit upperChanged();

    m_lower = timeSignature.lower;
    emit lowerChanged();
}
