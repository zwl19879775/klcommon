///
/// @file RefMember.h
/// @author Kevin Lynx
/// 
#ifndef ___REF_MEMBERS_H_
#define ___REF_MEMBERS_H_

#include "gvalue.h"
#include "GValueUtil.h"
#include <map>
#include <string>

// temp code
typedef long CGUID

/// RefMembers make a reference to a class data member, and wrap the value
/// setting/getting.
class RefMembers
{
private:
    class RefBase
    {
    public:
        virtual ~RefBase () {}
        virtual GValue Get () const = 0;
        virtual void Set (const GValue &val) = 0;
        virtual void Inc (double inc) = 0;
    };
    
    /// Default is number
    template <class RefT>
    struct Setter
    {
        static void Do (RefT *p, const GValue &val)
        {
            *p = val.GetNumber ();
        }
    };

    /// Boolean
    template <>
    struct Setter<bool>
    {
        static void Do (bool *p, const GValue &val)
        {
            *p = val.GetBool ();
        }
    };

    /// String
    template <>
    struct Setter<std::string>
    {
        static void Do (std::string *p, const GValue &val)
        {
            *p = CastString (val);
        }
    };
    
    /// GUID
    template <>
    struct Setter<CGUID>
    {
        static void Do (CGUID *p, const GValue &val)
        {
            *p = CastGUID (val);
        }
    };

    /// Only support number
    template<typename RefT>
    struct Incer
    {
        static void Do (RefT *p, double inc)
        {
            *p += (RefT) inc;
        }
    };

    /// new Ref<float> (&m_x)
    template <typename RefT>
    class Ref : public RefBase
    {
    public:
        Ref (RefT *obj) : m_obj (obj) { }

        GValue Get () const
        {
            return CreateGValue (*m_obj);
        }

        void Set (const GValue &val)
        {
            Setter<RefT>::Do (m_obj, val);
        }

        void Inc (double inc)
        {
            Incer<RefT>::Do (m_obj, inc);
        }
    private:
        RefT *m_obj;
    };
public:
    typedef std::map<std::string, RefBase*> MemberTable;
public:
    RefMembers () { }

    ~RefMembers ();

    /// Add a member reference.
    template <typename RefT>
    bool Add (const std::string &name, RefT *p);

    /// Remove a member reference.
    void Remove (const std::string &name);

    /// Set a member value.
    bool SetValue (const std::string &name, const GValue &val);

    /// Increase/decrease a member value, only support NUMBER.
    bool IncValue (const std::string &name, double inc);

    /// Get a member value. If not found, return nil.
    GValue GetValue (const std::string &name);
private:
    MemberTable m_mems;
};

template <typename RefT>
bool RefMembers::Add (const std::string &name, RefT *p)
{
    if (m_mems.find (name) != m_mems.end ())
    {
        return false;
    }
    m_mems[name] = new Ref<RefT> (p);
}

#endif

