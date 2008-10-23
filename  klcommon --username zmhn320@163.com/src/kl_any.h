///
/// @file kl_any.h
/// @author Kevin Lynx
/// @brief To implement a simple 'any' library, check out boost::any for more information.
///
#ifndef ___KL_ANY_H_
#define ___KL_ANY_H_


namespace kl_common
{
    ///
    /// any class can wrap any type in it and this can make your work more compatible.
    /// check out boost::any for more information.
    ///
    class any
    {
    public:
        any() : _t( 0 )
        {
        }
        
        template <typename _Tp>
        any( const _Tp &t ) : _t( new var_holder<_Tp>( t ) )
        {
        }

        any( const any &other ) : _t( other._t == 0 ? 0 : other._t->clone() )
        {
        }

        ~any()
        {
            delete _t;
        }

        any &operator = ( const any &other )
        {
            if( &other == this )
            {
                return *this;
            }
            delete _t;
            _t = other._t->clone();
            return *this;
        }

        bool empty() const
        {
            return _t == 0 ;
        }

    private:
        class base_type
        {
        public:
            virtual ~base_type()
            {
            }
            virtual base_type *clone() const = 0;
        };
        
        template <typename _Tp>
        class var_holder : public base_type
        {
        public:
            typedef _Tp type;
            typedef var_holder<type> self;
        public:
            var_holder( const type &t ) : _t( t )
            {
            }

            base_type *clone() const
            {
                return new self( _t );
            }
        public:
            type _t;
        };

    private:
        base_type *_t;
    private:
        template <typename _Tp>
        friend _Tp *any_cast( any* );
    };

    ///
    /// To cast any to the specified type.
    ///
    template <typename _Tp>
    _Tp *any_cast( any *t )
    {
        return t == 0 ? 0 : &static_cast<any::var_holder<_Tp>*>( t->_t )->_t;
    }

    template <typename _Tp>
    const _Tp *any_cast( const any *t )
    {
        return any_cast<_Tp>( const_cast<any*>( t ) );
    }

    ///
    /// If the t is not valid, this function may occur some crash.
    ///    
    template <typename _Tp>
    _Tp any_cast( any &t )
    {
        _Tp *ret = any_cast<_Tp>( &t );
        return *ret;
    }

    template <typename _Tp>
    _Tp any_cast( const any &t )
    {
        _Tp *ret = any_cast<_Tp>( const_cast<any*>( &t ) );
        return *ret;
    }

}

#endif // ___KL_ANY_H_

