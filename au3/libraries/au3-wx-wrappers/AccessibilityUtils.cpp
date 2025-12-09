/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AccessibilityUtils.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AccessibilityUtils.h"

#include <deque>
#include <tuple>
#include <type_traits>

#include <wx/window.h>

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/radiobut.h>
#include <wx/slider.h>
#include <wx/statbox.h>
#include <wx/textctrl.h>

#if wxUSE_ACCESSIBILITY
#   include "WindowAccessible.h"
#endif

template<typename ... Types>
bool IsOfType(wxWindow* window)
{
    std::tuple<std::add_pointer_t<Types>...> types;

    return std::apply(
        [window](auto... args) -> bool
    { return (dynamic_cast<decltype(args)>(window) != nullptr) || ...; },
        types);
}

void SetupAccessibility(wxWindow* window)
{
    std::deque<wxWindow*> elementsQueue;
    elementsQueue.push_back(window);

    while (!elementsQueue.empty())
    {
        auto element = elementsQueue.front();
        elementsQueue.pop_front();

        for (auto child : element->GetChildren()) {
            elementsQueue.push_back(child);
        }

#if wxUSE_ACCESSIBILITY
        if (element->GetAccessible() == nullptr) {
            if (IsOfType<
                    wxCheckBox, wxChoice, wxSlider, wxTextCtrl, wxStaticBox,
                    wxRadioButton>(element)) {
                element->SetAccessible(safenew WindowAccessible(element));
            }
        }
#endif

        const auto label = element->GetLabel();

        if (!label.empty()) {
            element->SetName(wxStripMenuCodes(label));
        }
    }
}
