/*  SPDX-License-Identifier: GPL-2.0-or-later */

#include "ProjectQMLEnvironment.h"
#include "QMLEngineFactory.h"
#include <QQmlContext>

using namespace audacity;

ProjectQMLEnvironment::Properties::Properties(ProjectQMLEnvironment& env)
   : mEnv { env }
{
   BuildAll();//Collect properties
}

ProjectQMLEnvironment::Property::Property(const QString& name, PropertyFactory factory)
   : mFactory([name, factory = std::move(factory)](auto& properties)
      { return CreateProperty(properties, name, factory); })
{
}

auto ProjectQMLEnvironment::Property::CreateProperty(Properties& properties,
   const QString& name, const PropertyFactory& factory) -> Properties::DataPointer
{
   const auto& engine = properties.mEnv.mEngine;
   auto property = factory(*engine, properties.mEnv.mProject);
   engine->setContextForObject(property.get(), engine->rootContext());
   engine->rootContext()->setContextProperty(name, property.get());
   return property;
}

ProjectQMLEnvironment::ProjectQMLEnvironment(AudacityProject& project)
   : mProject { project }
   , mEngine { QMLEngineFactory::Call() }
   , mProperties { *this }
{
   mEngine->setProperty("project", QVariant::fromValue(static_cast<void*>(&project)));
}

ProjectQMLEnvironment::~ProjectQMLEnvironment() = default;

AudacityProject& ProjectQMLEnvironment::GetProject() noexcept
{
   return mProject;
}

QQmlEngine& ProjectQMLEnvironment::GetEngine() noexcept
{
   return *mEngine;
}

//Creates a QML environment and attaches it to a new project.
static AudacityProject::AttachedObjects::RegisteredFactory sQMLEnvironment {
   [](AudacityProject& project)
   {
      return std::make_shared<ProjectQMLEnvironment>(project);
   }
};

ProjectQMLEnvironment& ProjectQMLEnvironment::Get(AudacityProject& project)
{
   return project.Get<ProjectQMLEnvironment>(sQMLEnvironment);
}

AudacityProject* ProjectQMLEnvironment::GetProject(QQmlEngine& engine)
{
   return static_cast<AudacityProject*>(engine.property("project").value<void*>());
}
