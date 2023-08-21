/*  SPDX-License-Identifier: GPL-2.0-or-later */

#include <QFontDatabase>
#include <QtCore/QDebug>
#include <QtQml/QQmlApplicationEngine>
#include <QtWidgets/QApplication>

int main(int argc, char *argv[])
{
   QGuiApplication app(argc, argv);

   QFontDatabase::addApplicationFont(":/fonts/MusescoreIcon.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Bold.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-BoldItalic.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Italic.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Regular.ttf");

   QQmlApplicationEngine engine;
   engine.addImportPath(QString(":%1").arg(AUDACITY_QML_RESOURCE_PREFIX));

   engine.load("qrc:/qml/main.qml");

   if (engine.rootObjects().isEmpty())
   {
      qDebug() << "Unable to load main.qml";
      return -1;
   }

   return app.exec();
}
