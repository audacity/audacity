#include "Ui.h"

#include <QQmlProperty>

#include "ProjectQMLEnvironment.h"
#include "Theme.h"

audacity::Ui::Ui()
   : mTheme(new Theme(this))
{
   BuildAll();
}

audacity::Theme* audacity::Ui::theme() const
{
   return mTheme;
}

void audacity::Ui::applyTheme(const QString& name)
{
   applyTheme(findTheme(name));
}

void audacity::Ui::applyTheme(const Theme* other)
{
   if(other == nullptr)
      return;

   auto meta = other->metaObject();
   for(int i = 0; i < meta->propertyCount(); ++i)
   {
      auto metaProperty = meta->property(i);
      if(!metaProperty.isWritable())
         continue;
      auto value = QQmlProperty::read(other, metaProperty.name());
      if(!value.isValid())
         continue;
      if(!QQmlProperty::write(mTheme, metaProperty.name(), value))
         qWarning() << "Unknown theme property: " << metaProperty.name();
   }
   emit themeChanged();
}

void audacity::Ui::registerTheme(Theme* theme)
{
   auto name = theme->name();
   int i = 1;
   while(mThemes.contains(name))
   {
      name = QString("%1 (%2)")
         .arg(theme->name())
         .arg(i);
      ++i;
   }
   mThemes[name] = theme;
   theme->setParent(this);

   emit availableThemesChanged();
}

QStringList audacity::Ui::availableThemes() const
{
   return mThemes.keys();
}

audacity::Theme* audacity::Ui::findTheme(const QString& name)
{
   const auto it = mThemes.find(name);
   if(it != mThemes.end())
      return it.value();
   return {};
}

static audacity::ProjectQMLEnvironment::Property ui {
   "ui",
   [](QQmlEngine& engine, AudacityProject& project) {
      return std::make_unique<audacity::Ui>();
   }
};

audacity::Ui& audacity::Ui::Get(AudacityProject& project)
{
   auto& env = ProjectQMLEnvironment::Get(project);
   return env.GetProperty<Ui>(ui);
}
