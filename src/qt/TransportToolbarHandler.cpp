#include <QDebug>
#include "TransportToolbarHandler.h"

TransportToolbarHandler::TransportToolbarHandler(QObject *parent)
   : QObject(parent)
{
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

