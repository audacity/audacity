/**********************************************************************

 Audacity: A Digital Audio Editor

 @file GlobalVariable.h

 Paul Licameli

 **********************************************************************/
#ifndef __AUDACITY_GLOBAL_VARIABLE__
#define __AUDACITY_GLOBAL_VARIABLE__

#include <functional>
#include <memory>
#include <optional>
#include <type_traits>
#include <utility>

//! Class template to generate global variables
/*!
 The variable is constructed when first used, regardless of the static
 initialization sequence of translation units.

 @tparam Tag distinguishes GlobalVariables with the same Type; often a "CRTP" argument
 @tparam Type must be non-reference, and default-constructible if there is
 no initializer function; if const-qualified, that means Get() gives
 non-mutating access, but Set() and Scope{...} are still possible if Type is
 movable
 @tparam initializer initial value, or a function that computes initial value
 @tparam ScopedOnly if true, then enforce RAII for changes of the variable
 (do not generate Set() or Scope::Commit())
 */
template<typename Tag, typename Type, auto initializer = nullptr,
         bool ScopedOnly = true>
class GlobalVariable
{
    struct dummy {
        explicit dummy()
        {
        }
    };
public:
    using variable_type = GlobalVariable;
    using stored_type = Type;
    using mutable_type = std::remove_const_t<Type>;

    //! Get the installed value
    static stored_type& Get()
    {
        // Force generation of non-inline Assign, in case it needs dllexport linkage
        &GlobalVariable::Assign;
        return Instance();
    }

    //! Move in a new value, move out and return the previous
    /*! Calls won't compile if ScopedOnly is true, though the function can
     still be generated at compile time.  (std::enable_if_t doesn't work here) */
    [[nodiscard]]
    static auto Set(std::conditional_t<ScopedOnly, dummy,
                                       mutable_type> replacement)  -> std::conditional_t<ScopedOnly, void,
                                                                                         mutable_type>
    {
        if constexpr (!ScopedOnly) {
            return Assign(std::move(replacement));
        }
    }

    //! RAII guard for temporary installation of a value; movable
    /*!
     Constructor captures previous value, installs a new value;
     destructor restores the previous value, unless moved-from or `Commit`ted

     Not required to have stack-like lifetime.  Non-nested lifetimes for
     distinct Scope objects is not prevented and may have surprising results.
     */
    class Scope
    {
    public:
        explicit Scope(mutable_type value)
            : m_previous{Assign(std::move(value))}
        {}
        Scope(Scope&& other) = default;
        Scope& operator=(Scope&& other) = default;
        ~Scope()
        {
            if constexpr (ScopedOnly) {
                Assign(std::move(m_previous));
            } else if (m_previous) {
                Assign(std::move(*m_previous));
            }
        }

        /*! Won't compile unless ScopedOnly is false */
        void Commit()
        {
            static_assert(!ScopedOnly);
            m_previous.reset();
        }

        bool HasValue() const
        {
            if constexpr (ScopedOnly) {
                return true;
            } else {
                return m_previous.has_value();
            }
        }

    private:
        std::conditional_t<ScopedOnly, mutable_type, std::optional<mutable_type> >
        m_previous;
    };

    /*! @brief Can guarantee that the global variable's lifetime encloses
     those of other objects of static duration

     This is like the "nifty counter" idiom.  The guarantee is not automatic,
     but you can define a `static` instance in a multiply included header file,
     which creates multiple Initializers, but only one of them causes the
     initial construction of the variable.

     This makes it safe for other calls to Get() to occur inside the bodies
     of both the constructor and destructor of another long-lived object.  Such
     objects can be freely defined by other translation units that include the
     header with no extra effort.
     */
    struct Initializer {
        Initializer()
        {
            Instance();
        }
    };

private:
    //! Use static functions only.  Don't directly construct this.
    GlobalVariable() = delete;

    //! Generate the static variable
    static mutable_type& Instance()
    {
        static_assert(!std::is_reference_v<stored_type>);
        if constexpr (std::is_convertible_v<
                          decltype(initializer), mutable_type
                          >) {
            static mutable_type instance{ initializer };
            return instance;
        } else if constexpr (initializer != nullptr) {
            static mutable_type instance{ initializer() };
            return instance;
        } else {
            static mutable_type instance;
            return instance;
        }
    }

    static mutable_type Assign(mutable_type&& replacement)
    {
        auto& instance = Instance();
        auto result = std::move(instance);
        instance = std::move(replacement);
        return result;
    }
};

//! Global function-valued variable, adding a convenient Call()
template<typename Tag, typename Signature, auto Default = nullptr,
         auto... Options>
class GlobalHook : public GlobalVariable<Tag,
                                         const std::function<Signature>, Default, Options...
                                         >
{
public:
    using result_type = typename std::function<Signature>::result_type;
    using Scope = typename GlobalVariable<Tag,
                                          const std::function<Signature>, Default, Options...
                                          >::Scope;

    //! Null check of the installed function is done for you
    /*! Requires that the return type of the function is void or
     default-constructible */
    template<typename ... Arguments>
    static result_type Call(Arguments&&... arguments)
    {
        auto& fn = GlobalHook::Get();
        if (fn) {
            return fn(std::forward<Arguments>(arguments)...);
        } else if constexpr (std::is_void_v<result_type>) {
            return;
        } else {
            return result_type{};
        }
    }

    //! Can generate overriding hooks of unique_ptr factories
    template<typename Derived>
    struct SubstituteInUnique : Scope {
        SubstituteInUnique()
            : Scope{
                    // Generic lambda is coerced to the needed pointer-to-function type
                    [](auto&&... args) -> result_type {
                return
                    std::make_unique<Derived>(std::forward<decltype(args)>(args)...);
            }
                    } {}
    };

    //! Can generate overriding hooks of shared_ptr factories
    template<typename Derived>
    struct SubstituteInShared : Scope {
        SubstituteInShared()
            : Scope{
                    // Generic lambda is coerced to the needed pointer-to-function type
                    [](auto&&... args) -> result_type {
                return
                    std::make_shared<Derived>(std::forward<decltype(args)>(args)...);
            }
                    } {}
    };
};

//! Global function-valued variable, with a default implementation given,
//! and adding a convenient Call()
/*!
 @tparam DefaultFunction names a function or static member function
 */
template<typename Tag, auto DefaultFunction, auto... Options>
class DefaultedGlobalHook : public GlobalHook< Tag,
                                               std::remove_pointer_t<decltype(DefaultFunction)>, DefaultFunction,
                                               Options...
                                               >
{
};

#endif
