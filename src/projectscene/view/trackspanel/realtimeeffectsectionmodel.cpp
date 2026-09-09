/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectsectionmodel.h"

#include <algorithm>
#include <utility>

using namespace muse;
using namespace au::projectscene;

RealtimeEffectSectionModel::RealtimeEffectSectionModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void RealtimeEffectSectionModel::load()
{
    configuration()->isEffectsPanelVisibleChanged().onNotify(this, [this]() {
        if (configuration()->isEffectsPanelVisible()) {
            savePreviouslyFocusedControl();
        } else {
            restorePreviouslyFocusedControl();
        }

        emit showEffectsSectionChanged();
    });

    dispatcher()->reg(this, "toggle-effects", [this] {
        toggleEffectsPanel();
    });

    dispatcher()->reg(this, "add-realtime-effects", [this] {
        toggleEffectsPanel();
    });

    emit showEffectsSectionChanged();
}

bool RealtimeEffectSectionModel::prop_showEffectsSection() const
{
    return configuration()->isEffectsPanelVisible();
}

void RealtimeEffectSectionModel::prop_setShowEffectsSection(bool show)
{
    configuration()->setIsEffectsPanelVisible(show);
}

bool RealtimeEffectSectionModel::prop_navigationFocusInsideEffectsPanel() const
{
    return m_navigationFocusInsideEffectsPanel;
}

void RealtimeEffectSectionModel::prop_setNavigationFocusInsideEffectsPanel(bool inside)
{
    if (m_navigationFocusInsideEffectsPanel == inside) {
        return;
    }

    m_navigationFocusInsideEffectsPanel = inside;
    emit navigationFocusInsideEffectsPanelChanged();
}

void RealtimeEffectSectionModel::toggleEffectsPanel()
{
    const muse::ui::INavigationSection* section = navigationController()->activeSection();
    if (section && section->type() == muse::ui::INavigationSection::Type::Exclusive) {
        return;
    }

    const bool shouldShow = !configuration()->isEffectsPanelVisible();
    configuration()->setIsEffectsPanelVisible(shouldShow);
    if (shouldShow) {
        emit focusEffectsPanelRequested();
    }
}

void RealtimeEffectSectionModel::savePreviouslyFocusedControl()
{
    m_previouslyFocusedControl = navigationController()->activeControl();
}

void RealtimeEffectSectionModel::restorePreviouslyFocusedControl()
{
    muse::ui::INavigationControl* const control = std::exchange(m_previouslyFocusedControl, nullptr);
    if (!control || !m_navigationFocusInsideEffectsPanel) {
        return;
    }

    const std::set<muse::ui::INavigationSection*>& sections = navigationController()->sections();
    const bool isRegistered = std::any_of(sections.cbegin(), sections.cend(), [control](const muse::ui::INavigationSection* section) {
        const std::set<muse::ui::INavigationPanel*>& panels = section->panels();

        return std::any_of(panels.cbegin(), panels.cend(), [control](const muse::ui::INavigationPanel* panel) {
            return panel->controls().count(control) > 0;
        });
    });

    if (!isRegistered) {
        return;
    }

    control->requestActive();
}
