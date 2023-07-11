#include <cassert>
#include <QtCore/QObject>
#include <QtCore/QString>
#include "EditToolbarHandler.h"
#include "uicomponents/ToolbarManager.h"

namespace
{
   // The ids must match those found in EditToolbar.qml
   const QString automationId{ "automation" };
   const QString zoomInId{ "zoomIn" };
   const QString zoomOutId{ "zoomOut" };
   const QString fitSelectionId{ "fitSelection" };
   const QString fitProjectId{ "fitProject" };
   const QString zoomToggleId{ "zoomToggle" };
   const QString trimId{ "trim" };
   const QString silenceId{ "silence" };
}

EditToolbarHandler::EditToolbarHandler(QObject *parent)
   : ToolbarHandler(parent)
{
   m_toolbarButtons[automationId] =
   {
      [this]() { return AutomationVisible(); },
      [this](bool isVisible) { SetAutomationVisible(isVisible); }
   };

   m_toolbarButtons[zoomInId] =
   {
      [this]() { return ZoomInVisible(); },
      [this](bool isVisible) { SetZoomInVisible(isVisible); }
   };

   m_toolbarButtons[zoomOutId] =
   {
      [this]() { return ZoomOutVisible(); },
      [this](bool isVisible) { SetZoomOutVisible(isVisible); }
   };

   m_toolbarButtons[fitSelectionId] =
   {
      [this]() { return FitSelectionVisible(); },
      [this](bool isVisible) { SetFitSelectionVisible(isVisible); }
   };

   m_toolbarButtons[fitProjectId] =
   {
      [this]() { return FitProjectVisible(); },
      [this](bool isVisible) { SetFitProjectVisible(isVisible); }
   };

   m_toolbarButtons[zoomToggleId] =
   {
      [this]() { return ZoomToggleVisible(); },
      [this](bool isVisible) { SetZoomToggleVisible(isVisible); }
   };

   m_toolbarButtons[trimId] =
   {
      [this]() { return TrimVisible(); },
      [this](bool isVisible) { SetTrimVisible(isVisible); }
   };

   m_toolbarButtons[silenceId] =
   {
      [this]() { return SilenceVisible(); },
      [this](bool isVisible) { SetSilenceVisible(isVisible); }
   };
}

void EditToolbarHandler::MonitorForConfigurationChanges()
{
   QObject::connect(m_toolbarManager, &ToolbarManager::visibleChanged,
                    this, &EditToolbarHandler::ToolbarButtonVisibilityHandler);
}

void EditToolbarHandler::RegisterToolbarConfiguration()
{
   assert(m_toolbarManager != nullptr);

   AddToolbarButtonConfiguration(automationId, tr("Automation"));
   m_toolbarManager->AddSeparator();
   AddToolbarButtonConfiguration(zoomInId, tr("Zoom in"));
   AddToolbarButtonConfiguration(zoomOutId, tr("Zoom out"));
   AddToolbarButtonConfiguration(fitSelectionId, tr("Fit selection to width"));
   AddToolbarButtonConfiguration(fitProjectId, tr("Fit project to width"));
   AddToolbarButtonConfiguration(zoomToggleId, tr("Zoom toggle"));
   m_toolbarManager->AddSeparator();
   AddToolbarButtonConfiguration(trimId, tr("Trim"));
   AddToolbarButtonConfiguration(silenceId, tr("Silence"));
   m_toolbarManager->AddSeparator();
}

bool EditToolbarHandler::AutomationVisible() const
{
   return m_automationVisible;
}

void EditToolbarHandler::SetAutomationVisible(bool isVisible)
{
   m_automationVisible = isVisible;
   UpdateToolbarVisibility();
}

QBindable<bool> EditToolbarHandler::BindableAutomationVisible()
{
   return &m_automationVisible;
}

bool EditToolbarHandler::ZoomInVisible() const
{
   return m_zoomInVisible;
}

void EditToolbarHandler::SetZoomInVisible(bool isVisible)
{
   m_zoomInVisible = isVisible;
   UpdateToolbarVisibility();
}

QBindable<bool> EditToolbarHandler::BindableZoomInVisible()
{
   return &m_zoomInVisible;
}

bool EditToolbarHandler::ZoomOutVisible() const
{
   return m_zoomOutVisible;
}

void EditToolbarHandler::SetZoomOutVisible(bool isVisible)
{
   m_zoomOutVisible = isVisible;
   UpdateToolbarVisibility();
}

QBindable<bool> EditToolbarHandler::BindableZoomOutVisible()
{
   return &m_zoomOutVisible;
}

bool EditToolbarHandler::FitSelectionVisible() const
{
   return m_fitSelectionVisible;
}

void EditToolbarHandler::SetFitSelectionVisible(bool isVisible)
{
   m_fitSelectionVisible = isVisible;
   UpdateToolbarVisibility();
}

QBindable<bool> EditToolbarHandler::BindableFitSelectionVisible()
{
   return &m_fitSelectionVisible;
}

bool EditToolbarHandler::FitProjectVisible() const
{
   return m_fitProjectVisible;
}

void EditToolbarHandler::SetFitProjectVisible(bool isVisible)
{
   m_fitProjectVisible = isVisible;
   UpdateToolbarVisibility();
}

QBindable<bool> EditToolbarHandler::BindableFitProjectVisible()
{
   return &m_fitProjectVisible;
}

bool EditToolbarHandler::ZoomToggleVisible() const
{
   return m_zoomToggleVisible;
}

void EditToolbarHandler::SetZoomToggleVisible(bool isVisible)
{
   m_zoomToggleVisible = isVisible;
   UpdateToolbarVisibility();
}

QBindable<bool> EditToolbarHandler::BindableZoomToggleVisible()
{
   return &m_zoomToggleVisible;
}

bool EditToolbarHandler::TrimVisible() const
{
   return m_trimVisible;
}

void EditToolbarHandler::SetTrimVisible(bool isVisible)
{
   m_trimVisible = isVisible;
   UpdateToolbarVisibility();
}

QBindable<bool> EditToolbarHandler::BindableTrimVisible()
{
   return &m_trimVisible;
}


bool EditToolbarHandler::SilenceVisible() const
{
   return m_silenceVisible;
}

void EditToolbarHandler::SetSilenceVisible(bool isVisible)
{
   m_silenceVisible = isVisible;
   UpdateToolbarVisibility();
}

QBindable<bool> EditToolbarHandler::BindableSilenceVisible()
{
   return &m_silenceVisible;
}

void EditToolbarHandler::Automation()
{
   emit updateStatusBar("Automation clicked");
}

void EditToolbarHandler::ZoomIn()
{
   emit updateStatusBar("Zoom in clicked");
}

void EditToolbarHandler::ZoomOut()
{
   emit updateStatusBar("Zoom out clicked");
}

void EditToolbarHandler::FitSelection()
{
   emit updateStatusBar("Zoom fit to selection clicked");
}

void EditToolbarHandler::FitProject()
{
   emit updateStatusBar("Zoom fit to project clicked");
}

void EditToolbarHandler::ZoomToggle()
{
   emit updateStatusBar("Zoom toggle clicked");
}

void EditToolbarHandler::Trim()
{
   emit updateStatusBar("Trim clicked");
}

void EditToolbarHandler::Silence()
{
   emit updateStatusBar("Silence clicked");
}
