/*  SPDX-License-Identifier: GPL-2.0-or-later */

#include <functional>
#include <optional>
#include <QDialogButtonBox>

#include "TranslatableString.h"

#include <QFontDatabase>
#include <QQmlComponent>
#include <QGuiApplication>
#include <QQuickWindow>
#include <QQmlProperty>
#include <QWindow>
#include <wx/log.h>

#include "AudioIO.h"
#include "BasicSettings.h"
#include "Prefs.h"
#include "Project.h"
#include "QMLEngineFactory.h"
#include "ProjectQMLEnvironment.h"
#include "CodeConversions.h"
#include "ExtraMenu.h"
#include "ProjectFileIO.h"
#include "ProjectHistory.h"
#include "QtQuickUiServices.h"
#include "trackpanel/TrackListModel.h"

#include "Track.h"

#include "ProjectSerializer.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "uithemes/UiTheme.h"

//Takes an ownership, ensures that window is properly deleted
class ProjectWindow final : public ClientData::Base
{
   AudacityProject& mProject;
   std::unique_ptr<QWindow> mWindow;
public:
   ProjectWindow(AudacityProject& project) : mProject(project)
   {
   }

   QWindow* GetWindow()
   {
      if(mWindow)
         return mWindow.get();

      QQmlComponent component {
         &audacity::ProjectQMLEnvironment::Get(mProject).GetEngine(),
         "qrc:/qml/main.qml"
      };
      auto obj = component.create();
      mWindow.reset(
         qobject_cast<QQuickWindow*>(obj)
      );
      if(!mWindow)
         throw std::runtime_error(component.errorString().toStdString());

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

static std::shared_ptr<AudacityProject> CreateProjectWindow()
{
   try
   {
      const auto project = AudacityProject::Create();
      
      ProjectWindow::GetWindow(*project);//request the window
      Projects::Get().push_back(project);
      return project;
   }
   catch(std::exception& e){
      BasicUI::ShowErrorDialog(
         {}, XO("Error"),
         XO("Failed to load applicaiton window:\n%s")
            .Format(e.what()),
         {});
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
   return {};
}

static ExtraMenu::Item openItem {
   XO("Open"),
   [](AudacityProject& project)
   {
      const auto window = ProjectWindow::GetWindow(project);
      const auto openProjectDialogObj = window->findChild<QObject*>("OpenProjectDialogObj");
      QMetaObject::invokeMethod(openProjectDialogObj, "open");
      while(true)
      {
         if(!QQmlProperty::read(openProjectDialogObj, "visible").toBool())
            break;
         BasicUI::Yield();
      }
      auto url = QQmlProperty::read(openProjectDialogObj, "currentFile").toUrl();

      auto openProject = [&](AudacityProject& project)
      {
         auto& projectFileIO = ProjectFileIO::Get(project);                                              
         auto result = projectFileIO.LoadProject(audacity::ToWXString(url.toString()), true);
         if(result)
            result->Commit();
      };

      if(ProjectHistory::Get(project).GetDirty() ||
         !TrackList::Get(project).empty())
      {
         const auto newProject = CreateProjectWindow();
         openProject(*newProject);
      }
      else
         openProject(project);
   }
};

/*static ExtraMenu::Item generateItem {
   XO("Generate"),
   [](AudacityProject& project)
   {
      auto& trackList = TrackList::Get(project);

      for(int i = 0; i < 10; ++i)
         trackList.Append(std::move(*WaveTrackFactory::Get(project).Create(1)));

      for(auto track : trackList.Any<WaveTrack>())
      {
         track->OnProjectTempoChange(120);
         track->InsertSilence(0.0, 1.0);
         auto clip = track->GetClips().back();
         clip->SetName("Silence");
      }

      for(auto track : trackList.Any<WaveTrack>())
      {
         auto copy = track->Copy(0, 1);
         for(int i = 1; i < 100; ++i)
            track->Paste(static_cast<double>(i), *copy);
      }
   }
};*/

static ExtraMenu::Item exitMenuItem {
   XO("&Exit"),
   [](AudacityProject&) {
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
   [](AudacityProject&) {
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
   [](AudacityProject&) {
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
   [](AudacityProject&) {
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
   [](AudacityProject&)
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
   [](QQmlEngine&, AudacityProject& project) {
      return std::make_unique<ExtraMenu>(project);
   }
};

static audacity::ProjectQMLEnvironment::Property projectTrackList {
   "projectTrackList",
   [](QQmlEngine& engine, AudacityProject& project) {
      return std::make_unique<TrackListModel>(
         TrackList::Get(project).shared_from_this(),
         &engine);
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
   wxLogNull suppressLog; 	

   QtQuickUiServices::Get();//install
   QGuiApplication app(argc, argv);

   UiTheme::Register();

   QFontDatabase::addApplicationFont(":/fonts/MusescoreIcon.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Bold.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-BoldItalic.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Italic.ttf");
   QFontDatabase::addApplicationFont(":/fonts/Lato-Regular.ttf");

   if (!ProjectFileIO::InitializeSQL())
   {
      BasicUI::ShowMessageBox(
         XO("SQLite library failed to initialize.  Audacity cannot continue.")
      );
      return 0;
   }

   InitPreferences(audacity::ApplicationSettings::Call());
   AudioIO::Init();

   //cleans up everything when leave the scope
   Projects::Scope projects{{}};
   
   if(!CreateProjectWindow())
      return 0;

   auto result = app.exec();
   //close all opened projects
   for(auto project : Projects::Get())
   {
      auto &projectFileIO = ProjectFileIO::Get(*project);
      projectFileIO.CloseProject();
   }
   return result;
}
