/*  SPDX-License-Identifier: GPL-2.0-or-later */

#include "TranslatableString.h"

#include <QFontDatabase>
#include <QQmlComponent>
#include <QGuiApplication>
#include <QQuickWindow>
#include <QWindow>

#include "AudioIO.h"
#include "BasicSettings.h"
#include "Prefs.h"
#include "Project.h"
#include "QMLEngineFactory.h"
#include "ProjectQMLEnvironment.h"
#include "CodeConversions.h"
#include "ExtraMenu.h"
#include "QtQuickUiServices.h"
#include "uithemes/UiTheme.h"

//Takes an ownership, ensures that window is properly deleted
class ProjectWindow final : public ClientData::Base
{
   std::unique_ptr<QWindow> mWindow;
public:
   ProjectWindow(AudacityProject& project)
   {
      QQmlComponent component {
         &audacity::ProjectQMLEnvironment::Get(project).GetEngine(),
         "qrc:/qml/main.qml"
      };
      mWindow.reset(
         qobject_cast<QQuickWindow*>(component.create())
      );
      if(!mWindow)
         throw std::runtime_error(component.errorString().toStdString());
   }

   QWindow* GetWindow() const
   {
      return mWindow.get();
   }

   static ProjectWindow& Get(AudacityProject& project);

   static QWindow* GetWindow(AudacityProject& project)
   {
      return Get(project).GetWindow();
   }
};

struct Projects final : GlobalVariable<Projects, std::vector<std::shared_ptr<AudacityProject>>> { };

static AudacityProject::AttachedObjects::RegisteredFactory sProjectWindow {
   [](AudacityProject& project) {
      return std::make_unique<ProjectWindow>(project);
   }
};

ProjectWindow& ProjectWindow::Get(AudacityProject& project)
{
   return project.Get<ProjectWindow>(sProjectWindow);
}

static bool CreateProjectWindow()
{
   auto project = AudacityProject::Create();
   try
   {
      ProjectWindow::Get(*project);//request the window
   }
   catch(std::exception& e){
      BasicUI::ShowErrorDialog(
         {}, XO("Error"),
         XO("Failed to load applicaiton window:\n%s")
            .Format(e.what()),
         {});
      return false;
   }
   //Ideally, delete the project and everything when window is closed
   //, but it does not compile as there are undefines...
   /*QObject::connect(window, &QQuickWindow::closing,
      [window](QQuickCloseEvent*) {
         for(const auto& project : Projects::Get())
         {
            if(window == ProjectWindow::GetWindow(*project))
            {
               Projects::Get().Remove(*project);
               break;
            }
         }
      }
   );*/
   Projects::Get().push_back(project);
   return true;
}

static ExtraMenu::Item newWindowItem {
   XO("New window"),
   [] {
      CreateProjectWindow();
   }
};

static ExtraMenu::Item exitMenuItem {
   XO("&Exit"),
   [] {
      using namespace BasicUI;
      CallAfter([]{
         if(ShowMessageBox(XO("Are you sure you want to exit?"),
            MessageBoxOptions{}
               .ButtonStyle(Button::YesNo)
               .IconStyle(Icon::Question)) == MessageBoxResult::Yes)
         {
            if(const auto focusWindow = QGuiApplication::focusWindow())
               focusWindow->close();
         }
      });
   }
};

static ExtraMenu::Item showProgress {
   XO("Show progress"),
   [] {
      using namespace BasicUI;
      CallAfter([] {
         using namespace std::chrono;
         const auto progress = MakeProgress(XO("Progress dialog"),
            XO("This is application modal progress dialog"),
            ProgressConfirmStopOrCancel | ProgressShowStop);
         const auto startTime = system_clock::now();
         while(true)
         {
            constexpr milliseconds::rep progressDuration = 15000; 
            const auto now = system_clock::now();
            const auto elapsed = std::min(
               duration_cast<milliseconds>(now - startTime).count(),
               progressDuration);
            if(progress->Poll(elapsed, progressDuration) != ProgressResult::Success)
               break;
            if(elapsed == progressDuration)
               break;
            Yield();//Process the event loop
         }
      });
   }
};

static ExtraMenu::Item showGenericProgress {
   XO("Show generic progress"),
   [] {
      using namespace BasicUI;
      CallAfter([] {
         using namespace std::chrono;
         const auto progress = MakeGenericProgress(*FindFocus(),
            XO("Progress dialog"),
            XO("This is project window modal generic progress dialog"));
         const auto startTime = system_clock::now();
         while(true)
         {
            constexpr milliseconds::rep progressDuration = 15000; 
            const auto now = system_clock::now();
            const auto elapsed = std::min(
               duration_cast<milliseconds>(now - startTime).count(),
               progressDuration);
            progress->Pulse();
            if(elapsed == progressDuration)
               break;
            Yield();//Process the event loop
         }
      });
   }
};

static ExtraMenu::Item showGenericProgressOrphaned {
   XO("Show generic progress (application modal)"),
   [] {
      using namespace BasicUI;
      CallAfter([] {
         using namespace std::chrono;
         const auto progress = MakeGenericProgress({},
            XO("Progress dialog"),
            XO("This is application modal generic progress dialog"));
         const auto startTime = system_clock::now();
         while(true)
         {
            constexpr milliseconds::rep progressDuration = 15000; 
            const auto now = system_clock::now();
            const auto elapsed = std::min(
               duration_cast<milliseconds>(now - startTime).count(),
               progressDuration);
            progress->Pulse();
            if(elapsed == progressDuration)
               break;
            Yield();//Process event loop
         }
      });
   }
};

static ExtraMenu::Item showMultiDialog {
   XO("MultiDialog"),
   []
   {
      using namespace BasicUI;
      CallAfter([]
      {
         auto buttons = TranslatableStrings { XO("Option 1"), XO("Option 2"), XO("Option 3") };
         auto result = ShowMultiDialog(
            XO("Application modal dialog that offers multiple choices"),
            XO("MultiDialog"),
            buttons, "", XO("Options"), false);
         ShowMessageBox(XO("Your choice is %s").Format(buttons[result]));
      });
   }
};

static audacity::ProjectQMLEnvironment::Property extraMenu {
   "extraMenu",
   [](QQmlEngine&, AudacityProject&) {
      return std::make_unique<ExtraMenu>();
   }
};

class StubSettings final : public audacity::BasicSettings
{
public:
   wxString GetGroup() const override { return ""; }
   wxArrayString GetChildGroups() const override { return {}; }
   wxArrayString GetChildKeys() const override { return {}; }
   bool HasEntry(const wxString& key) const override { return false; }
   bool HasGroup(const wxString& key) const override { return false; }
   bool Remove(const wxString& key) override { return false; }
   void Clear() override { }
   bool Read(const wxString& key, bool* value) const override { return false; }
   bool Read(const wxString& key, int* value) const override { return false; }
   bool Read(const wxString& key, long* value) const override { return false; }
   bool Read(const wxString& key, long long* value) const override { return false; }
   bool Read(const wxString& key, double* value) const override { return false; }
   bool Read(const wxString& key, wxString* value) const override { return false; }
   bool Write(const wxString& key, bool value) override { return false; }
   bool Write(const wxString& key, int value) override { return false; }
   bool Write(const wxString& key, long value) override { return false; }
   bool Write(const wxString& key, long long value) override { return false; }
   bool Write(const wxString& key, double value) override { return false; }
   bool Write(const wxString& key, const wxString& value) override { return false; }
   bool Flush() noexcept override { return false; }

protected:
   void DoBeginGroup(const wxString& prefix) override { }
   void DoEndGroup() noexcept override { }
};

audacity::ApplicationSettings::Scope applicationSettings {
   []{ return std::make_unique<StubSettings>(); }
};

static audacity::QMLEngineFactory::Scope qmlEngineFactory {
   [] {
      auto engine = std::make_unique<QQmlEngine>();
      engine->addImportPath(QString(":%1").arg(AUDACITY_QML_RESOURCE_PREFIX));
      const auto uiTheme = UiTheme::Get(*engine);
      uiTheme->applyTheme(uiTheme->themes()[0]);
      return engine;
   }
};

int main(int argc, char *argv[])
{
   QtQuickUiServices::Get();//install
   QGuiApplication app(argc, argv);

   UiTheme::Register();

   QFontDatabase::addApplicationFont(":/fonts/MusescoreIcon.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Bold.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-BoldItalic.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Italic.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Regular.ttf");

   InitPreferences(audacity::ApplicationSettings::Call());
   AudioIO::Init();

   //cleans up everything when leave the scope
   Projects::Scope projects{{}};
   
   if(!CreateProjectWindow())
      return 0;

   return app.exec();
}
