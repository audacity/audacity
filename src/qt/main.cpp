/*  SPDX-License-Identifier: GPL-2.0-or-later */

#include <QtCore/QDebug>
#include <QFontDatabase>
#include <QQmlComponent>
#include <QGuiApplication>

#include "BasicSettings.h"
#include "Project.h"
#include "QMLEngineFactory.h"
#include "ProjectQMLEnvironment.h"

static audacity::QMLEngineFactory::Scope qmlEngineFactory {
   [] {
      auto engine = std::make_unique<QQmlEngine>();
      engine->addImportPath(QString(":%1").arg(AUDACITY_QML_RESOURCE_PREFIX));
      return engine;
   }
};

int main(int argc, char *argv[])
{
   QGuiApplication app(argc, argv);

   QFontDatabase::addApplicationFont(":/fonts/MusescoreIcon.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Bold.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-BoldItalic.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Italic.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Regular.ttf");

   auto project = AudacityProject::Create();
   auto& engine = audacity::ProjectQMLEnvironment::Get(*project).GetEngine();

   QQmlComponent applicationWindowComponent (&engine, "qrc:/qml/main.qml");
   auto applicationWindow = std::unique_ptr<QObject>(applicationWindowComponent.create());
   if(applicationWindow == nullptr)
   {
      qDebug() << "Unable to load main.qml: " <<
         applicationWindowComponent.errorString();
      return -1;
   }

   return app.exec();
}
