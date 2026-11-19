/*
 * Audacity: A Digital Audio Editor
 */
#include "clipfadeseditmodel.h"

#include <algorithm>

#include "fadetypeconversion.h"
#include "framework/global/realfn.h"
#include "framework/global/translation.h"

using namespace au::projectscene;

namespace {
constexpr double MIXED_LENGTH = -1.0;
constexpr int MIXED_TYPE = -1;

double commonLength(const au::trackedit::Clips& clips, double au::trackedit::Clip::* length)
{
    if (clips.empty()) {
        return MIXED_LENGTH;
    }
    const double first = clips.front().*length;
    const bool allEqual = std::all_of(clips.begin(), clips.end(), [&](const au::trackedit::Clip& clip) {
        return muse::RealIsEqual(clip.*length, first);
    });
    return allEqual ? first : MIXED_LENGTH;
}

int commonType(const au::trackedit::Clips& clips, au::trackedit::FadeType au::trackedit::Clip::* type)
{
    if (clips.empty()) {
        return MIXED_TYPE;
    }
    const auto first = clips.front().*type;
    const bool allEqual = std::all_of(clips.begin(), clips.end(), [&](const au::trackedit::Clip& clip) {
        return clip.*type == first;
    });
    return allEqual ? static_cast<int>(toFadeTypesType(first)) : MIXED_TYPE;
}
}

ClipFadesEditModel::ClipFadesEditModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void ClipFadesEditModel::load()
{
    m_clipKeys = selectionController()->selectedClips();
    if (m_clipKeys.empty()) {
        return;
    }

    // So that a cancel can roll the preview back
    projectHistory()->startUserInteraction();
    m_editing = true;

    emit fadesChanged();
}

void ClipFadesEditModel::apply()
{
    if (!m_editing) {
        return;
    }
    if (m_changed) {
        projectHistory()->pushHistoryState(muse::trc("projectscene", "Edited fades"),
                                           muse::trc("projectscene", "Edited fades"));
    }
    endEdit();
}

void ClipFadesEditModel::cancel()
{
    if (!m_editing) {
        return;
    }
    if (m_changed) {
        projectHistory()->rollbackState();
        if (trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject()) {
            prj->reload();
        }
    }
    endEdit();
}

void ClipFadesEditModel::endEdit()
{
    // No-op if a state was pushed or rolled back
    projectHistory()->endUserInteraction();
    m_editing = false;
    m_changed = false;
}

au::trackedit::Clips ClipFadesEditModel::clips() const
{
    trackedit::Clips result;
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return result;
    }
    for (const trackedit::ClipKey& key : m_clipKeys) {
        trackedit::Clip clip = prj->clip(key);
        if (clip.isValid()) {
            result.push_back(std::move(clip));
        }
    }
    return result;
}

void ClipFadesEditModel::change(const std::function<void(const trackedit::ClipKey&)>& changeClip)
{
    if (!m_editing) {
        return;
    }
    for (const trackedit::ClipKey& key : m_clipKeys) {
        changeClip(key);
    }
    m_changed = true;
    emit fadesChanged();
}

double ClipFadesEditModel::fadeInLength() const
{
    return commonLength(clips(), &trackedit::Clip::fadeInDuration);
}

void ClipFadesEditModel::setFadeInLength(double length)
{
    change([&](const trackedit::ClipKey& key) {
        trackeditInteraction()->changeClipFadeIn(key, length, false);
    });
}

int ClipFadesEditModel::fadeInType() const
{
    return commonType(clips(), &trackedit::Clip::fadeInType);
}

void ClipFadesEditModel::setFadeInType(int type)
{
    change([&](const trackedit::ClipKey& key) {
        trackeditInteraction()->changeClipFadeInType(key, toTrackeditFadeType(static_cast<FadeTypes::Type>(type)), false);
    });
}

double ClipFadesEditModel::fadeOutLength() const
{
    return commonLength(clips(), &trackedit::Clip::fadeOutDuration);
}

void ClipFadesEditModel::setFadeOutLength(double length)
{
    change([&](const trackedit::ClipKey& key) {
        trackeditInteraction()->changeClipFadeOut(key, length, false);
    });
}

int ClipFadesEditModel::fadeOutType() const
{
    return commonType(clips(), &trackedit::Clip::fadeOutType);
}

void ClipFadesEditModel::setFadeOutType(int type)
{
    change([&](const trackedit::ClipKey& key) {
        trackeditInteraction()->changeClipFadeOutType(key, toTrackeditFadeType(static_cast<FadeTypes::Type>(type)), false);
    });
}

double ClipFadesEditModel::maxFadeLength() const
{
    // Each clip clamps the length to its own duration
    double result = 0.0;
    for (const trackedit::Clip& clip : clips()) {
        result = std::max(result, clip.endTime - clip.startTime);
    }
    return result;
}

QString ClipFadesEditModel::clipTitle() const
{
    const trackedit::Clips edited = clips();
    return edited.size() == 1 ? edited.front().title.toQString() : QString();
}

int ClipFadesEditModel::clipCount() const
{
    return static_cast<int>(clips().size());
}
