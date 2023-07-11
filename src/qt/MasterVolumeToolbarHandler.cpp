#include <cassert>
#include <QtCore/QObject>
#include <QtCore/QString>
#include "MasterVolumeToolbarHandler.h"
#include "uicomponents/ToolbarManager.h"

namespace
{
   // The ids must match those found in MasterVolumeToolbar.qml
   const QString volumeControlId{ "volumeControl" };
}

MasterVolumeToolbarHandler::MasterVolumeToolbarHandler(QObject *parent)
   : ToolbarHandler(parent)
{
   m_toolbarButtons[volumeControlId] =
   {
      [this]() { return MasterVolumeVisible(); },
      [this](bool isVisible) { SetMasterVolumeVisible(isVisible); }
   };
}

void MasterVolumeToolbarHandler::MonitorForConfigurationChanges()
{
   QObject::connect(m_toolbarManager, &ToolbarManager::visibleChanged,
                    this, &MasterVolumeToolbarHandler::ToolbarButtonVisibilityHandler);
}

void MasterVolumeToolbarHandler::RegisterToolbarConfiguration()
{
   assert(m_toolbarManager != nullptr);

   AddToolbarButtonConfiguration(volumeControlId, tr("Volume meter"));
}

bool MasterVolumeToolbarHandler::MasterVolumeVisible() const
{
   return m_masterVolumeVisible;
}

void MasterVolumeToolbarHandler::SetMasterVolumeVisible(bool isVisible)
{
   m_masterVolumeVisible = isVisible;
   UpdateToolbarVisibility();
}

QBindable<bool> MasterVolumeToolbarHandler::BindableMasterVolumeVisible()
{
   return &m_masterVolumeVisible;
}
