#pragma once
#include <QObject>
#include <QtQmlIntegration/QtQmlIntegration>

#include "ClientData.h"
#include "Theme.h"

class AudacityProject;

namespace audacity
{
   class Ui;
   using AttachedUiObjects = ClientData::Site<Ui, QObject>;

   class Ui final : public QObject, public AttachedUiObjects
   {
      Q_OBJECT

      Q_PROPERTY(audacity::Theme* theme READ theme NOTIFY themeChanged FINAL)
      Q_PROPERTY(QStringList availableThemes READ availableThemes NOTIFY availableThemesChanged FINAL)

      Theme* mTheme{};

      QMap<QString, Theme*> mThemes;

   public:

      Ui();

      Theme* theme() const;
      Q_INVOKABLE void applyTheme(const QString& name);
      void applyTheme(const Theme* other);

      void registerTheme(Theme* theme);

      QStringList availableThemes() const;

      Theme* findTheme(const QString& name);

   signals:
      void availableThemesChanged();
      void themeChanged();

   public:

      ///!This call may have side effect of creating QQmlEngine instance,
      ///!but considering that Ui instance needs QQmlEngine as a host object
      ///!that should not give undesired consequences.
      static Ui& Get(AudacityProject& project);
   };

}
