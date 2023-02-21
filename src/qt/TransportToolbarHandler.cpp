#include <QDebug>
#include "TransportToolbarHandler.h"

TransportToolbarHandler::TransportToolbarHandler(QObject *parent)
   : QObject(parent)
{
}

bool TransportToolbarHandler::playing() const
{
   return m_isPlaying;
}

void TransportToolbarHandler::setPlaying(bool state)
{
   if (m_isPlaying != state)
   {
      m_isPlaying = state;
      emit playStateChanged(m_isPlaying);
   }
}

void TransportToolbarHandler::play()
{
   setPlaying(!playing());
   emit updateStatusBar(playing() ? "Play clicked" : "Pause clicked");
}

void TransportToolbarHandler::stop()
{
   emit updateStatusBar("Stop clicked");
}

void TransportToolbarHandler::rewind()
{
   emit updateStatusBar("Rewind clicked");
}

void TransportToolbarHandler::fastForward()
{
   emit updateStatusBar("Fast forward clicked");
}

void TransportToolbarHandler::record()
{
   emit updateStatusBar("Record clicked");
}

void TransportToolbarHandler::loop()
{
   emit updateStatusBar("Loop clicked");
}

