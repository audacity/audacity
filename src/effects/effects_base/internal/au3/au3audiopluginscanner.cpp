/*
 * Audacity: A Digital Audio Editor
 */

#include "au3audiopluginscanner.h"

#include <chrono>

#include <QCoreApplication>
#include <QEventLoop>

#include "au3-basic-ui/BasicUI.h"
#include "au3-module-manager/PluginManager.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "framework/global/io/dir.h"
#include "framework/global/log.h"
#include "framework/global/progress.h"

namespace au::effects {
namespace {
//! Adapter exposing a muse::Progress as a BasicUI::ProgressDialog, so Audacity
//! backend modules (VST3 / AU / LV2) which expect a real BasicUI::ProgressDialog*
//! can drive cancellation through the host-supplied muse::Progress.
class FrameworkProgressAsBasicUIDialog final : public BasicUI::ProgressDialog
{
public:
    explicit FrameworkProgressAsBasicUIDialog(muse::Progress* inner)
        : m_inner{inner} {}

    BasicUI::ProgressResult Poll(unsigned long long numerator, unsigned long long denominator,
                                 const ::TranslatableString& message) override
    {
        if (!m_inner) {
            return BasicUI::ProgressResult::Success;
        }

        m_inner->progress(static_cast<int64_t>(numerator), static_cast<int64_t>(denominator),
                          message.translated().toStdString());

        // The scan runs synchronously on the main thread, so we must pump the
        // event loop periodically to let the host's progress dialog repaint and
        // the cancel button respond.
        using clock = std::chrono::steady_clock;
        constexpr auto pumpInterval = std::chrono::milliseconds(100);
        const auto now = clock::now();
        if (now - m_lastEventPump >= pumpInterval) {
            m_lastEventPump = now;
            QCoreApplication::processEvents(QEventLoop::AllEvents);
        }

        return m_inner->isCanceled() ? BasicUI::ProgressResult::Cancelled : BasicUI::ProgressResult::Success;
    }

    void SetMessage(const ::TranslatableString&) override {}
    void SetDialogTitle(const ::TranslatableString&) override {}
    void Reinit() override {}

private:
    muse::Progress* m_inner { nullptr };
    std::chrono::steady_clock::time_point m_lastEventPump {};
};
}

Au3AudioPluginScanner::Au3AudioPluginScanner(PluginProvider& provider)
    : m_pluginProvider{provider}
{
}

void Au3AudioPluginScanner::init(muse::IApplication::RunMode mode)
{
    m_runMode = mode;
    m_pluginProvider.Initialize();
    doInit();
}

void Au3AudioPluginScanner::deinit()
{
    m_pluginProvider.Terminate();
}

muse::io::paths_t Au3AudioPluginScanner::scanPlugins(muse::Progress* progress) const
{
    // Push user-configured custom paths into PluginManager so that providers
    // (e.g. VST3) which read them via ReadCustomPaths(*this) pick them up.
    syncCustomPathsToProvider();

    muse::io::paths_t result;

    // Backend modules (VST3 / AU / LV2) accept a BasicUI::ProgressDialog*, so wrap the
    // framework-side progress sink in a BasicUI::ProgressDialog adapter for the duration of the scan.
    FrameworkProgressAsBasicUIDialog wrappedDialog{ progress };
    BasicUI::ProgressDialog* dialogPtr = progress ? &wrappedDialog : nullptr;

    const PluginPaths paths = pluginPaths(dialogPtr);
    for (const auto& path : paths) {
        const auto modulePath = path.BeforeFirst(';');
        auto convertedPath = muse::io::Dir::fromNativeSeparators(au3::wxToString(modulePath));
        result.emplace_back(std::move(convertedPath));
    }

    return result;
}

PluginPaths Au3AudioPluginScanner::pluginPaths(BasicUI::ProgressDialog* progress) const
{
    // PluginManager still needed here by some effect modules for the implementation of custom paths.
    return m_pluginProvider.FindModulePaths(PluginManager::Get(), progress);
}

void Au3AudioPluginScanner::syncCustomPathsToProvider() const
{
    // Must NOT run during plugin-registration subprocesses
    // (`--register-audio-plugin`), which never call this code path and
    // don't load IEffectsConfiguration / PluginManager::Initialize().
    // Catching the case here guards against future callers wiring up an
    // init/scan sequence without setting the run mode first.
    IF_ASSERT_FAILED(m_runMode.has_value() && *m_runMode != muse::IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    const muse::io::paths_t userPaths = customPaths();
    ::PluginPaths customPathsForProvider;
    customPathsForProvider.reserve(userPaths.size());
    for (const auto& p : userPaths) {
        if (!p.empty()) {
            customPathsForProvider.emplace_back(wxString::FromUTF8(p.toStdString()));
        }
    }
    PluginManager::Get().StoreCustomPaths(m_pluginProvider, customPathsForProvider);
}
}
