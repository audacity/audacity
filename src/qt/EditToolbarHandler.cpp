#include <QDebug>
#include "EditToolbarHandler.h"

EditToolbarHandler::EditToolbarHandler(QObject *parent)
   : QObject(parent)
{
}

void EditToolbarHandler::automation()
{
   emit updateStatusBar("Automation clicked");
}

void EditToolbarHandler::zoomIn()
{
   emit updateStatusBar("Zoom in clicked");
}

void EditToolbarHandler::zoomOut()
{
   emit updateStatusBar("Zoom out clicked");
}

void EditToolbarHandler::fitSelection()
{
   emit updateStatusBar("Zoom fit to selection clicked");
}

void EditToolbarHandler::fitProject()
{
   emit updateStatusBar("Zoom fit to project clicked");
}

void EditToolbarHandler::zoomToggle()
{
   emit updateStatusBar("Zoom toggle clicked");
}

void EditToolbarHandler::trim()
{
   emit updateStatusBar("Trim clicked");
}

void EditToolbarHandler::silence()
{
   emit updateStatusBar("Silence clicked");
}
