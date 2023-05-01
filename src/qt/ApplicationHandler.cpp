#include <QtGui/QGuiApplication>
#include <QtQml/QQmlEngine>
#include "ApplicationHandler.h"

ApplicationHandler::ApplicationHandler(QObject *parent)
   : QObject(parent)
{
   (void) translator.load(":/translations/de");
}

void ApplicationHandler::ChangeToEnglish()
{
   qApp->removeTranslator(&translator);
   qmlEngine(this)->retranslate();
}

void ApplicationHandler::ChangeToGerman()
{
   qApp->installTranslator(&translator);
   qmlEngine(this)->retranslate();
}
