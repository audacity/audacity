#include <QDebug>
#include "ToolsToolbarHandler.h"

ToolsToolbarHandler::ToolsToolbarHandler(QObject *parent)
   : QObject(parent)
{
}

void ToolsToolbarHandler::Setup()
{
   emit updateStatusBar("Setup clicked");
}
