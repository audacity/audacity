/*
* Audacity: A Digital Audio Editor
*/
#include "trackeditconfiguration.h"

#include "settings.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "au3-wave-track/WaveTrack.h"

namespace au::trackedit {
static const std::string moduleName("trackedit");

static const muse::Settings::Key ASK_BEFORE_CONVERTING_TO_MONO_OR_STEREO(moduleName, "trackedit/askBeforeConvertingToMonoOrStereo");
static const muse::Settings::Key PASTE_AS_NEW_CLIP(moduleName, "trackedit/pasteAsNewClip");
static const muse::Settings::Key DELETE_BEHAVIOR(moduleName, "trackedit/deleteBehavior");
static const muse::Settings::Key CLOSE_GAP_BEHAVIOR(moduleName, "trackedit/closeGapBehavior");
static const muse::Settings::Key PASTE_BEHAVIOR(moduleName, "trackedit/pasteBehavior");
static const muse::Settings::Key PASTE_INSERT_BEHAVIOR(moduleName, "trackedit/pasteInsertBehavior");
// Empty means "use the translated default name"
static const muse::Settings::Key DEFAULT_AUDIO_TRACK_NAME(moduleName, "trackedit/defaultAudioTrackName");

void TrackeditConfiguration::init()
{
    muse::settings()->setDefaultValue(PASTE_AS_NEW_CLIP, muse::Val(true));
    muse::settings()->valueChanged(PASTE_AS_NEW_CLIP).onReceive(nullptr, [this](const muse::Val&) {
        m_pasteAsNewClipChanged.notify();
    });

    muse::settings()->setDefaultValue(ASK_BEFORE_CONVERTING_TO_MONO_OR_STEREO, muse::Val(true));
    muse::settings()->valueChanged(ASK_BEFORE_CONVERTING_TO_MONO_OR_STEREO).onReceive(nullptr, [this](const muse::Val&) {
        m_askBeforeConvertingToMonoOrStereoChanged.notify();
    });

    muse::settings()->setDefaultValue(DELETE_BEHAVIOR, muse::Val(DeleteBehavior::NotSet));
    muse::settings()->valueChanged(DELETE_BEHAVIOR).onReceive(nullptr, [this](const muse::Val&) {
        m_deleteBehaviorChanged.notify();
    });

    muse::settings()->setDefaultValue(CLOSE_GAP_BEHAVIOR, muse::Val(CloseGapBehavior::ClipRipple));
    muse::settings()->valueChanged(CLOSE_GAP_BEHAVIOR).onReceive(nullptr, [this](const muse::Val&) {
        m_closeGapBehaviorChanged.notify();
    });

    muse::settings()->setDefaultValue(PASTE_BEHAVIOR, muse::Val(PasteBehavior::PasteOverlap));
    muse::settings()->valueChanged(PASTE_BEHAVIOR).onReceive(nullptr, [this](const muse::Val&) {
        m_pasteBehaviorChanged.notify();
    });

    muse::settings()->setDefaultValue(PASTE_INSERT_BEHAVIOR, muse::Val(PasteInsertBehavior::PasteInsert));
    muse::settings()->valueChanged(PASTE_INSERT_BEHAVIOR).onReceive(nullptr, [this](const muse::Val&) {
        m_pasteInsertBehaviorChanged.notify();
    });

    muse::settings()->setDefaultValue(DEFAULT_AUDIO_TRACK_NAME, muse::Val(std::string()));
    muse::settings()->valueChanged(DEFAULT_AUDIO_TRACK_NAME).onReceive(nullptr, [this](const muse::Val&) {
        applyDefaultAudioTrackName();
        m_defaultAudioTrackNameChanged.notify();
    });
    applyDefaultAudioTrackName();
}

void TrackeditConfiguration::applyDefaultAudioTrackName() const
{
    // New tracks are named by au3 code (WaveTrack::GetDefaultAudioTrackNamePreference),
    // so keep its setting in sync with ours
    AudioTrackNameSetting.Write(au3::wxFromStdString(muse::settings()->value(DEFAULT_AUDIO_TRACK_NAME).toString()));
}

std::string TrackeditConfiguration::defaultAudioTrackName() const
{
    return au3::wxToStdString(WaveTrack::GetDefaultAudioTrackNamePreference());
}

void TrackeditConfiguration::setDefaultAudioTrackName(const std::string& name)
{
    const std::string trimmed = muse::String::fromStdString(name).trimmed().toStdString();
    if (muse::settings()->value(DEFAULT_AUDIO_TRACK_NAME).toString() == trimmed) {
        return;
    }
    muse::settings()->setSharedValue(DEFAULT_AUDIO_TRACK_NAME, muse::Val(trimmed));
}

muse::async::Notification TrackeditConfiguration::defaultAudioTrackNameChanged() const
{
    return m_defaultAudioTrackNameChanged;
}

bool TrackeditConfiguration::askBeforeConvertingToMonoOrStereo() const
{
    return muse::settings()->value(ASK_BEFORE_CONVERTING_TO_MONO_OR_STEREO).toBool();
}

void TrackeditConfiguration::setAskBeforeConvertingToMonoOrStereo(bool ask)
{
    if (askBeforeConvertingToMonoOrStereo() == ask) {
        return;
    }
    muse::settings()->setSharedValue(ASK_BEFORE_CONVERTING_TO_MONO_OR_STEREO, muse::Val(ask));
}

muse::async::Notification TrackeditConfiguration::askBeforeConvertingToMonoOrStereoChanged() const
{
    return m_askBeforeConvertingToMonoOrStereoChanged;
}

DeleteBehavior TrackeditConfiguration::deleteBehavior() const
{
    return muse::settings()->value(DELETE_BEHAVIOR).toEnum<DeleteBehavior>();
}

void TrackeditConfiguration::setDeleteBehavior(DeleteBehavior value)
{
    if (deleteBehavior() == value) {
        return;
    }
    muse::settings()->setSharedValue(DELETE_BEHAVIOR, muse::Val(value));
    m_deleteBehaviorChanged.notify();
}

muse::async::Notification TrackeditConfiguration::deleteBehaviorChanged() const
{
    return m_deleteBehaviorChanged;
}

CloseGapBehavior TrackeditConfiguration::closeGapBehavior() const
{
    return muse::settings()->value(CLOSE_GAP_BEHAVIOR).toEnum<CloseGapBehavior>();
}

void TrackeditConfiguration::setCloseGapBehavior(CloseGapBehavior value)
{
    if (closeGapBehavior() == value) {
        return;
    }
    muse::settings()->setSharedValue(CLOSE_GAP_BEHAVIOR, muse::Val(value));
    m_closeGapBehaviorChanged.notify();
}

muse::async::Notification TrackeditConfiguration::closeGapBehaviorChanged() const
{
    return m_closeGapBehaviorChanged;
}

PasteBehavior TrackeditConfiguration::pasteBehavior() const
{
    return muse::settings()->value(PASTE_BEHAVIOR).toEnum<PasteBehavior>();
}

void TrackeditConfiguration::setPasteBehavior(PasteBehavior value)
{
    if (pasteBehavior() == value) {
        return;
    }
    muse::settings()->setSharedValue(PASTE_BEHAVIOR, muse::Val(value));
    m_pasteBehaviorChanged.notify();
}

muse::async::Notification TrackeditConfiguration::pasteBehaviorChanged() const
{
    return m_pasteBehaviorChanged;
}

PasteInsertBehavior TrackeditConfiguration::pasteInsertBehavior() const
{
    return muse::settings()->value(PASTE_INSERT_BEHAVIOR).toEnum<PasteInsertBehavior>();
}

void TrackeditConfiguration::setPasteInsertBehavior(PasteInsertBehavior value)
{
    if (pasteInsertBehavior() == value) {
        return;
    }
    muse::settings()->setSharedValue(PASTE_INSERT_BEHAVIOR, muse::Val(value));
    m_pasteInsertBehaviorChanged.notify();
}

muse::async::Notification TrackeditConfiguration::pasteInsertBehaviorChanged() const
{
    return m_pasteInsertBehaviorChanged;
}

bool TrackeditConfiguration::pasteAsNewClip() const
{
    return muse::settings()->value(PASTE_AS_NEW_CLIP).toBool();
}

void TrackeditConfiguration::setPasteAsNewClip(bool value)
{
    if (pasteAsNewClip() == value) {
        return;
    }

    muse::settings()->setSharedValue(PASTE_AS_NEW_CLIP, muse::Val(value));
}

muse::async::Notification TrackeditConfiguration::pasteAsNewClipChanged() const
{
    return m_pasteAsNewClipChanged;
}
}
