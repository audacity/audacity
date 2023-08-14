#include "UiThemeHandler.h"

#include <QtCore/QFileInfo>
#include <QtCore/QDirIterator>

QStringList UiThemeHandler::themeFiles()
{
   QStringList themes;
   QStringList excludeFiles{ "Theme.qml", "UiTheme.qml" };

   QDirIterator it(
      QString(":%1/Audacity/UiThemes")
         .arg(uithemes_QML_IMPORT_PREFIX),
      { "*.qml" },
      QDir::Files,
      QDirIterator::Subdirectories);
   while (it.hasNext())
   {
      auto path = it.next();
      auto fileName = QFileInfo(path).fileName();

      if (!excludeFiles.contains(fileName))
         themes << fileName;
   }

   return themes;
}
