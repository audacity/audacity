#include <cassert>
#include <QtCore/QObject>
#include <QtCore/QString>
#include "TimeToolbarHandler.h"
#include "uicomponents/ToolbarManager.h"

namespace
{
   // The ids must match those found in TimeToolbar.qml
   const QString timeControlId{ "projectTime" };
}

TimeToolbarHandler::TimeToolbarHandler(QObject *parent)
   : ToolbarHandler(parent)
{
   m_toolbarButtons[timeControlId] =
   {
         [this]() { return TimeControlVisible(); },
         [this](bool isVisible) { SetTimeControlVisible(isVisible); }
   };
}

void TimeToolbarHandler::MonitorForConfigurationChanges()
{
   QObject::connect(m_toolbarManager, &ToolbarManager::visibleChanged,
                    this, &TimeToolbarHandler::ToolbarButtonVisibilityHandler);
}

void TimeToolbarHandler::RegisterToolbarConfiguration()
{
   assert(m_toolbarManager != nullptr);

   AddToolbarButtonConfiguration(timeControlId, tr("Timecode"));
   m_toolbarManager->AddSeparator();
}

bool TimeToolbarHandler::TimeControlVisible() const
{
   return m_timeControlVisible;
}

void TimeToolbarHandler::SetTimeControlVisible(bool isVisible)
{
   m_timeControlVisible = isVisible;
   UpdateToolbarVisibility();
}

QBindable<bool> TimeToolbarHandler::BindableTimeControlVisible()
{
   return &m_timeControlVisible;
}
