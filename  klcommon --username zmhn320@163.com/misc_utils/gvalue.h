///
/// @file gvalue.h
/// @brief A generic value wrapper for some basic c/c++ types.
/// @author Kevin Lynx
/// @date 5.6.2011
///
#ifndef ___gvalue_h_
#define ___gvalue_h_

/// GValue has a copy-on-write mechanism to make copy operation more effective:
///     GValue val = val2;  no copy operation
/// NOTE: If you want to add some c++ class object in this Value, i strongly
/// suggest you add the pointer which point to the object, instead the object itself.
class GValue {
public:
    /// Basic value type. Extend type can >= RAW.
    enum {
        NIL, NUMBER, BOOLEAN, RAW
    };

    /// Raw memory, it can store `string` `guid` etc.
    struct Raw {
        void *p;
        size_t size;
    };

    /// Represents the real value.
    union Value {
        int n;
        double d;
        Raw r;
    };
public:
    /// Initialize a nil value.
    GValue () { 
        Init ();
        Set (); 
    }

    GValue (double d) { 
        Init ();
        Set (d); 
    }

    GValue (bool b) { 
        Init ();
        Set (b); 
    }

    GValue (const void *p, size_t size, int t) { 
        Init ();
        Set (p, size, t); 
    }

    GValue (const GValue &other);

    ~GValue ();

    GValue &operator = (const GValue &other) ;

    bool operator == (const GValue &other) const;

    void Set ();
    void Set (double d);
    void Set (bool b);
    void Set (const void *p, size_t size, int t);

    /// If the type does not match, it will return 0 whatever the cast type is.
    bool GetBool () const;
    double GetNumber() const;
    const Raw *GetRaw() const;

    int Type () const {
        return m_type;
    }

    bool IsRaw () const {
        return m_type >= RAW;
    }

private:
    void Copy (const GValue &other);
    void Init ();    
    void Free ();
    void FreeVal ();
    void IncRef () const;
    void DecRef ();
    void Split ();
private:
    int *m_ref; // because of the `const`
    int m_type;
    Value m_val;
};

#endif

