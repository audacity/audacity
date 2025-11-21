/**********************************************************************

Audacity: A Digital Audio Editor

ProjectStatus.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_PROJECT_STATUS__
#define __AUDACITY_PROJECT_STATUS__
#endif

#include <utility>
#include <unordered_map>
#include <vector>
#include "ClientData.h" // to inherit
#include "Prefs.h"
#include "Observer.h"
#include "Registry.h"

class AudacityProject;
class wxWindow;

using StatusBarField = Identifier;

//! ID of the first field in the status bar. This filed is used to display playback state.
PROJECT_API StatusBarField StateStatusBarField();
//! ID of the second field in the status bar. This field is expandable.
PROJECT_API StatusBarField MainStatusBarField();
//! ID of the third field in the status bar. This field is used to display the current rate.
PROJECT_API StatusBarField RateStatusBarField();

//! Abstract base class for status bar fields
class PROJECT_API StatusBarFieldItem /* not final */ : public Registry::SingleItem
{
public:
    explicit StatusBarFieldItem(StatusBarField identifier);
    virtual ~StatusBarFieldItem();

    /*! Return the default width of the field in pixels.
     * This is used to determine the initial width of the field in the status bar.
     * The value may be overridden using StatusWidthFunctions.
     * Returning -1 means that the field is expandable.In this case StatusWidthFunctions
     * have no effect.
     */
    virtual int GetDefaultWidth(const AudacityProject& project) const = 0;

    /*! Called when the status bar associated with the project is resized.
     * Could be used to update the position of the custom status bar field.
     * Default implementation does nothing.
     *
     * @param project The project whose status bar has been resized. The reference is non constant, as
     *                the field is likely implemented as a client site.
     */
    virtual void OnSize(AudacityProject& project);

    //! Sets the current text of the field.
    virtual void SetText(AudacityProject& project, const TranslatableString& msg) = 0;
    //! Retrieves the current text of the field.
    virtual TranslatableString GetText(const AudacityProject& project) const = 0;

    //! Returns true if the field is visible in the status bar of the given project.
    virtual bool IsVisible(const AudacityProject& project) const = 0;

protected:
    //! Derived classes should call this method to notify the status bar that the field has changed.
    void DispatchFieldChanged(const AudacityProject& project);
};

struct PROJECT_API StatusBarFieldRegistryTraits final : Registry::DefaultTraits
{
    using LeafTypes = List<StatusBarFieldItem>;
};

using StatusBarFieldRegistryVisitor = Registry::VisitorFunctions<StatusBarFieldRegistryTraits>;

//! Registry of status bar fields
struct PROJECT_API ProjectStatusFieldsRegistry final
{
    //! Returns the registry
    static Registry::GroupItem<StatusBarFieldRegistryTraits>& Registry();
    //! Visits all fields in the registry in order
    static void Visit(const StatusBarFieldRegistryVisitor& visitor);
    //! Returns the number of fields in the registry. If project is no null, only visible fields are counted.
    static std::size_t Count(const AudacityProject* project);
    //! Returns the field with the given identifier or nullptr if field is not present
    static StatusBarFieldItem* Get(const StatusBarField& identifier);
    //! Returns the zero based index of the field or -1 if field is not present
    static int GetFieldIndex(
        const AudacityProject& project, const StatusBarField& identifier);
    //! Handle OnSize event for all fields in the registry
    static void OnSize(AudacityProject& project);

    static Observer::Subscription
    Subscribe(std::function<void(const AudacityProject&, const StatusBarField&)>
              handler);
};

using StatusBarFieldItemRegistrator
    =Registry::RegisteredItem<ProjectStatusFieldsRegistry>;

class PROJECT_API ProjectStatus final : public ClientData::Base, public PrefsListener, public Observer::Publisher<StatusBarField>
{
public:
    static ProjectStatus& Get(AudacityProject& project);
    static const ProjectStatus& Get(const AudacityProject& project);

    explicit ProjectStatus(AudacityProject& project);
    ProjectStatus(const ProjectStatus&) = delete;
    ProjectStatus& operator=(const ProjectStatus&) = delete;
    ~ProjectStatus() override;

    // Type of a function to report translatable strings, and also report an extra
    // margin, to request that the corresponding field of the status bar should
    // be wide enough to contain any of those strings plus the margin.
    using StatusWidthResult = std::pair< std::vector<TranslatableString>, unsigned >;
    using StatusWidthFunction = std::function<
        StatusWidthResult (const AudacityProject&, StatusBarField)
        >;
    using StatusWidthFunctions = std::vector< StatusWidthFunction >;

    // Typically a static instance of this struct is used.
    struct PROJECT_API RegisteredStatusWidthFunction
    {
        explicit
        RegisteredStatusWidthFunction(const StatusWidthFunction& function);
    };

    static const StatusWidthFunctions& GetStatusWidthFunctions();

    TranslatableString Get(StatusBarField field = MainStatusBarField()) const;
    void Set(const TranslatableString& msg, StatusBarField field = MainStatusBarField());

    // PrefsListener implementation
    void UpdatePrefs() override;

private:
    class ProjectStatusTextField;
    static struct DefaultFieldsRegistrator final
    {
        DefaultFieldsRegistrator();
    } sDefaultFieldsRegistrator;

    AudacityProject& mProject;
    std::unordered_map<StatusBarField, TranslatableString> mCurrentStatus;
    Observer::Subscription mFieldChangedSubscription;
};
