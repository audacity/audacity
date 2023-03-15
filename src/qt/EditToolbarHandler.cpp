#include <QDebug>
#include "EditToolbarHandler.h"

EditToolbarHandler::EditToolbarHandler(QObject *parent)
   : QObject(parent)
{
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
