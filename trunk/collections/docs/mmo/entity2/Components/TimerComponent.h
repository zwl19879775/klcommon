///
/// @file TimerComponent.h
/// @author Kevin Lynx
///
#ifndef ___TIMER_COMPONENT_H_
#define ___TIMER_COMPONENT_H_

#include "../IComponent.h"
#include <string>
#include <map>

/// Wrap timer stuff. Every component which want to register a timer, can use this
/// component to help it.
class TimerComponent : public IComponent /*, public GameEvent */
{
public:
    /// Actually, the real callback is not the type below, it's XXComponent::Func (void*);
    typedef void (*Callback) (IComponent*, void *arg);
    struct Timer 
    {
        void *arg;
        long id;
        IComponent *obj;
        Callback fn;
    };
    typedef std::map<std::string, Timer> TimerTable;
public:
    TimerComponent (Entity *entity) : IComponent (entity, "TimerComponent") { }

    /// Unregister all registered timers.
    virtual ~TimerComponent ();

    /// Register a timer for a component, the callback function must be the prototype:
    /// void XXComponent::Func (void*)
    template <typename Fn>
    bool Register (const std::string &name, IComponent *obj, Fn fn, void *arg, int first, int inter);

    /// Unregister a timer by its name.
    void UnRegister (const std::string &name);

    /// Unregister all timers.
    void UnRegisterAll ();

    /// TODO: adapte to GameEvent
private:
    /// Make the timer expired callback.
    void DoCallback (const std::string &name);    

    /// Hide the register implementation details.
    bool DoRegister (const std::string &name, IComponent *obj, Callback fn, void *arg);
private:
    TimerTable m_timers;
};

template <typename Fn>
bool TimerComponent::Register (const std::string &name, IComponent *obj, Fn fn, void *arg, int first, int inter)
{
    return DoRegister (name, obj, (Callback) fn, arg, first, inter);
}

#endif

