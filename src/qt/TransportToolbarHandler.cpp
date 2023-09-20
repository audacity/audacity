#include <cassert>
#include <QtCore/QObject>
#include <QtCore/QString>
#include "TransportToolbarHandler.h"
#include "uicomponents/ToolbarManager.h"

namespace
{
   // The ids must match those found in TransportToolbar.qml
   const QString playId{ "play" };
   const QString stopId{ "stop" };
   const QString recordId{ "record" };
   const QString rewindId{ "rewind" };
   const QString fastForwardId{ "fastForward" };
   const QString loopId{ "loop" };
}

TransportToolbarHandler::TransportToolbarHandler(QObject* parent)
   : ToolbarHandler(parent)
{
   m_toolbarButtons[playId] =
   {
      [this]() { return PlayVisible(); },
      [this](bool isVisible) { SetPlayVisible(isVisible); }
   };

   m_toolbarButtons[stopId] =
   {
      [this]() { return StopVisible(); },
      [this](bool isVisible) { SetStopVisible(isVisible); }
   };

   m_toolbarButtons[recordId] =
   {
      [this]() { return RecordVisible(); },
      [this](bool isVisible) { SetRecordVisible(isVisible); }
   };

   m_toolbarButtons[rewindId] =
   {
      [this]() { return RewindVisible(); },
      [this](bool isVisible) { SetRewindVisible(isVisible); }
   };

   m_toolbarButtons[fastForwardId] =
   {
      [this]() { return FastForwardVisible(); },
      [this](bool isVisible) { SetFastForwardVisible(isVisible); }
   };

   m_toolbarButtons[loopId] =
   {
      [this]() { return LoopVisible(); },
      [this](bool isVisible) { SetLoopVisible(isVisible); }
   };
}

void TransportToolbarHandler::MonitorForConfigurationChanges()
{
   QObject::connect(m_toolbarManager, &ToolbarManager::visibleChanged, this,
                    &TransportToolbarHandler::ToolbarButtonVisibilityHandler);
}

void TransportToolbarHandler::RegisterToolbarConfiguration()
{
   assert(m_toolbarManager != nullptr);

   AddToolbarButtonConfiguration(playId, tr("Play"));
   AddToolbarButtonConfiguration(stopId, tr("Stop"));
   AddToolbarButtonConfiguration(recordId, tr("Record"));
   AddToolbarButtonConfiguration(rewindId, tr("Step backwards"));
   AddToolbarButtonConfiguration(fastForwardId, tr("Step forwards"));
   AddToolbarButtonConfiguration(loopId, tr("Loop"));
   m_toolbarManager->AddSeparator();
}

bool TransportToolbarHandler::Playing() const
{
   return m_isPlaying;
}

void TransportToolbarHandler::SetPlaying(bool state)
{
   if (m_isPlaying != state)
   {
      m_isPlaying = state;
      emit playStateChanged(m_isPlaying);
   }
}

bool TransportToolbarHandler::PlayVisible() const
{
   return m_playVisible;
}

void TransportToolbarHandler::SetPlayVisible(bool isVisible)
{
   m_playVisible = isVisible;
   UpdateToolbarVisibility();
}

QBindable<bool> TransportToolbarHandler::BindablePlayVisible()
{
   return &m_playVisible;
}

bool TransportToolbarHandler::StopVisible() const
{
   return m_stopVisible;
}

void TransportToolbarHandler::SetStopVisible(bool isVisible)
{
   m_stopVisible = isVisible;
   UpdateToolbarVisibility();
}

QBindable<bool> TransportToolbarHandler::BindableStopVisible()
{
   return &m_stopVisible;
}

bool TransportToolbarHandler::RecordVisible() const
{
   return m_recordVisible;
}

void TransportToolbarHandler::SetRecordVisible(bool isVisible)
{
   m_recordVisible = isVisible;
   UpdateToolbarVisibility();
}

QBindable<bool> TransportToolbarHandler::BindableRecordVisible()
{
   return &m_recordVisible;
}

bool TransportToolbarHandler::RewindVisible() const
{
   return m_rewindVisible;
}

void TransportToolbarHandler::SetRewindVisible(bool isVisible)
{
   m_rewindVisible = isVisible;
   UpdateToolbarVisibility();
}

QBindable<bool> TransportToolbarHandler::BindableRewindVisible()
{
   return &m_rewindVisible;
}

bool TransportToolbarHandler::FastForwardVisible() const
{
   return m_fastForwardVisible;
}

void TransportToolbarHandler::SetFastForwardVisible(bool isVisible)
{
   m_fastForwardVisible = isVisible;
   UpdateToolbarVisibility();
}

QBindable<bool> TransportToolbarHandler::BindableFastForwardVisible()
{
   return &m_fastForwardVisible;
}

bool TransportToolbarHandler::LoopVisible() const
{
   return m_loopVisible;
}

void TransportToolbarHandler::SetLoopVisible(bool isVisible)
{
   m_loopVisible = isVisible;
   UpdateToolbarVisibility();
}

QBindable<bool> TransportToolbarHandler::BindableLoopVisible()
{
   return &m_loopVisible;
}

void TransportToolbarHandler::Play()
{
   SetPlaying(!Playing());
}

void TransportToolbarHandler::Stop()
{
   SetPlaying(false);
   emit playbackStopped();
}

void TransportToolbarHandler::Rewind()
{
   emit updateStatusBar("Rewind clicked");
}

void TransportToolbarHandler::FastForward()
{
   emit updateStatusBar("Fast forward clicked");
}

void TransportToolbarHandler::Record()
{
   emit updateStatusBar("Record clicked");
}

void TransportToolbarHandler::Loop()
{
   emit updateStatusBar("Loop clicked");
}
