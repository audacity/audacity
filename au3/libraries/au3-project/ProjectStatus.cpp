/**********************************************************************

Audacity: A Digital Audio Editor

ProjectStatus.h

Paul Licameli

**********************************************************************/

#include "ProjectStatus.h"

#include <algorithm>

#include "Project.h"

#include "AppEvents.h"
#include "BasicUI.h"

static const AudacityProject::AttachedObjects::RegisteredFactory key{
    []( AudacityProject& parent ){
        return std::make_shared< ProjectStatus >(parent);
    }
};

ProjectStatus& ProjectStatus::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get< ProjectStatus >(key);
}

const ProjectStatus& ProjectStatus::Get(const AudacityProject& project)
{
    return Get(const_cast< AudacityProject& >(project));
}

ProjectStatus::DefaultFieldsRegistrator ProjectStatus::sDefaultFieldsRegistrator;

ProjectStatus::ProjectStatus(AudacityProject& project)
    : mProject{project}
    , mFieldChangedSubscription{ProjectStatusFieldsRegistry::Subscribe(
                                    [this](const auto& project, const auto& field)
    {
        if (&mProject == &project)
            Publish(field);
    })}
{
}

ProjectStatus::~ProjectStatus() = default;

namespace {
ProjectStatus::StatusWidthFunctions& statusWidthFunctions()
{
    static ProjectStatus::StatusWidthFunctions theFunctions;
    return theFunctions;
}
}

ProjectStatus::RegisteredStatusWidthFunction::RegisteredStatusWidthFunction(
    const StatusWidthFunction& function)
{
    statusWidthFunctions().emplace_back(function);
}

auto ProjectStatus::GetStatusWidthFunctions() -> const StatusWidthFunctions&
{
    return statusWidthFunctions();
}

TranslatableString ProjectStatus::Get(StatusBarField field) const
{
    auto fieldObject = ProjectStatusFieldsRegistry::Get(field);
    return fieldObject != nullptr && fieldObject->IsVisible(mProject)
           ? fieldObject->GetText(mProject)
           : TranslatableString {};
}

void ProjectStatus::Set(const TranslatableString& msg, StatusBarField field)
{
    auto fieldObject = ProjectStatusFieldsRegistry::Get(field);
    if (fieldObject != nullptr) {
        fieldObject->SetText(mProject, msg);
    }
}

void ProjectStatus::UpdatePrefs()
{
    ProjectStatusFieldsRegistry::Visit([this](const StatusBarFieldItem& item, const auto&)
    { Publish(item.name); });
}

StatusBarField StateStatusBarField()
{
    return L"state";
}

StatusBarField MainStatusBarField()
{
    return L"main";
}

StatusBarField RateStatusBarField()
{
    return L"rate";
}

namespace {
const auto PathStart = L"StatusBarFieldRegistry";

struct Message final
{
    const AudacityProject& project;
    const StatusBarField& identifier;
};

struct Dispatcher final : Observer::Publisher<Message>
{
    void
    Dispatch(const AudacityProject& project, const StatusBarField& identifier)
    {
        Publish(Message { project, identifier });
    }

    Observer::Subscription
    Subscribe(std::function<void(const AudacityProject&, const StatusBarField&)>
              handler)
    {
        return Observer::Publisher<Message>::Subscribe(
            [handler = std::move(handler)](const auto& message)
        { handler(message.project, message.identifier); });
    }

    static Dispatcher& Get()
    {
        static Dispatcher dispatcher;
        return dispatcher;
    }

    // This method is always called from the main thread
    void NewFieldRegistered(const StatusBarField& identifier)
    {
        mCallEnqueued = true;
        mNewFields.push_back(identifier);

        AppEvents::OnAppInitialized(
            [this]
        {
            if (mCallEnqueued) {
                BasicUI::CallAfter(
                    [this]
                {
                    for (auto& project : AllProjects {}) {
                        for (const auto& field : mNewFields) {
                            Dispatch(*project, field);
                        }
                    }

                    mNewFields.clear();
                    mCallEnqueued = false;
                });
            }
        });
    }

private:
    std::vector<StatusBarField> mNewFields;
    bool mCallEnqueued{ false };
}; // struct Dispatcher
} // namespace

StatusBarFieldItem::StatusBarFieldItem(StatusBarField identifier)
    : SingleItem{identifier}
{
    Dispatcher::Get().NewFieldRegistered(identifier);
}

StatusBarFieldItem::~StatusBarFieldItem()
{
}

void StatusBarFieldItem::OnSize(AudacityProject&)
{
}

void StatusBarFieldItem::DispatchFieldChanged(const AudacityProject& project)
{
    Dispatcher::Get().Dispatch(project, name);
}

Registry::GroupItem<StatusBarFieldRegistryTraits>&
ProjectStatusFieldsRegistry::Registry()
{
    static Registry::GroupItem<StatusBarFieldRegistryTraits> registry {
        PathStart
    };
    return registry;
}

void ProjectStatusFieldsRegistry::Visit(
    const StatusBarFieldRegistryVisitor& visitor)
{
    static Registry::OrderingPreferenceInitializer init {
        PathStart,
        { { L"", L"state,main,rate" } },
    };

    Registry::GroupItem<StatusBarFieldRegistryTraits> top { PathStart };
    Registry::VisitWithFunctions(visitor, &top, &Registry());
}

std::size_t ProjectStatusFieldsRegistry::Count(const AudacityProject* project)
{
    if (project == nullptr) {
        return Registry().size();
    }

    auto& group = Registry();

    std::size_t count = 0;
    Visit(
        [&](const StatusBarFieldItem& item, const auto&)
    {
        if (item.IsVisible(*project)) {
            ++count;
        }
    });

    return count;
}

StatusBarFieldItem* ProjectStatusFieldsRegistry::Get(const StatusBarField& identifier)
{
    StatusBarFieldItem* result = nullptr;

    Visit(
        [&](const StatusBarFieldItem& item, const auto&)
    {
        if (item.name == identifier) {
            result = const_cast<StatusBarFieldItem*>(&item);
        }
    });

    return result;
}

int ProjectStatusFieldsRegistry::GetFieldIndex(
    const AudacityProject& project, const StatusBarField& identifier)
{
    int result = -1;
    int currentIndex = 1;
    Visit(
        [&](const StatusBarFieldItem& item, const auto&)
    {
        if (!item.IsVisible(project)) {
            return;
        }

        if (item.name == identifier) {
            result = currentIndex;
        }
        ++currentIndex;
    });

    return result;
}

void ProjectStatusFieldsRegistry::OnSize(AudacityProject& project)
{
    Visit([&](const StatusBarFieldItem& item, const auto&)
    { const_cast<StatusBarFieldItem&>(item).OnSize(project); });
}

Observer::Subscription ProjectStatusFieldsRegistry::Subscribe(
    std::function<void(const AudacityProject&, const StatusBarField&)> handler)
{
    return Dispatcher::Get().Subscribe(std::move(handler));
}

class ProjectStatus::ProjectStatusTextField final : public StatusBarFieldItem
{
public:
    ProjectStatusTextField(StatusBarField identifier, int defaultWidth)
        : StatusBarFieldItem{identifier}
        , mDefaultWidth{defaultWidth}
    {
    }

    ~ProjectStatusTextField() override = default;

    void SetText(AudacityProject& project, const TranslatableString& text) override
    {
        auto& projectStatus = ProjectStatus::Get(project);

        auto it = projectStatus.mCurrentStatus.find(name);

        if (it == projectStatus.mCurrentStatus.end()) {
            projectStatus.mCurrentStatus.emplace(name, text);
            DispatchFieldChanged(project);
            return;
        }

        if (it->second.Translation() != text.Translation()) {
            it->second = text;
            DispatchFieldChanged(project);
        }
    }

    TranslatableString GetText(const AudacityProject& project) const
    {
        auto& projectStatus = ProjectStatus::Get(project);

        auto it = projectStatus.mCurrentStatus.find(name);
        return it != projectStatus.mCurrentStatus.end() ? it->second
               : TranslatableString {};
    }

    int GetDefaultWidth(const AudacityProject&) const override
    {
        return mDefaultWidth;
    }

    bool IsVisible(const AudacityProject&) const override
    {
        return true;
    }

private:
    const int mDefaultWidth;
}; // class ProjectStatusTextField

ProjectStatus::DefaultFieldsRegistrator::DefaultFieldsRegistrator()
{
    static StatusBarFieldItemRegistrator stateStatusBarField {
        std::make_unique<ProjectStatusTextField>(StateStatusBarField(), 0),
        { {}, { Registry::OrderingHint::Begin } }
    };

    static StatusBarFieldItemRegistrator mainStatusBarField {
        // Main field is always expandable, hence the size is -1
        std::make_unique<ProjectStatusTextField>(MainStatusBarField(), -1),
        { {}, { Registry::OrderingHint::After, StateStatusBarField().GET() } }
    };

    static StatusBarFieldItemRegistrator rateStatusBarField {
        std::make_unique<ProjectStatusTextField>(RateStatusBarField(), 150),
        { {}, { Registry::OrderingHint::After, MainStatusBarField().GET() } }
    };
}
