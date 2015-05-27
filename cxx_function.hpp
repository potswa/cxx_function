// cxx_function.hpp: major evolution for std::function
// Copyright 2015 by David Krauss.
// This source is released under the MIT license, http://opensource.org/licenses/MIT

#ifndef INCLUDED_CXX_FUNCTION_HPP
#define INCLUDED_CXX_FUNCTION_HPP

#include <exception>
#include <functional> // only for std::bad_function_call
#include <memory>
#include <new>
#include <tuple>
#include <type_traits>
#include <utility>

namespace cxx_function {

template< typename ... sig >
class function;

template< typename ... sig >
class unique_function;

template< typename alloc, typename ... sig >
class function_container;

template< typename alloc, typename ... sig >
class unique_function_container;

template< typename >
struct any_piecewise_construct_tag {};

namespace impl {

/* Implement a vtable using metaprogramming. Why?
    1. Implement without polymorphic template instantiations (would need 2N of them).
    2. Eliminate overhead and ABI issues associated with RTTI and weak linkage.
    3. Allow static data entries as well as functions.
*/
enum class dispatch_slot {
    destructor,
    move_constructor,
    copy_constructor,
    base_index
};
constexpr int operator + ( dispatch_slot e ) { return static_cast< int >( e ); }

// "Abstract" base class for the island inside the wrapper class, e.g. std::function.
// This must appear first in the derived class layout.
template< typename ... sig >
struct erasure_base {
    typedef std::tuple<
        void (erasure_base::*)(), // destructor
        void (erasure_base::*)( void * ) &&, // move constructor
        void (erasure_base::*)( void * ) const &, // copy constructor
        
        sig erasure_base::* ... // dispatchers
    > dispatch_table;
    
    dispatch_table const & table;
    
    constexpr erasure_base( dispatch_table const & in_table )
        : table( in_table ) {}
};

// Generic "virtual" functions to manage the wrapper payload lifetime.
template< typename derived >
struct erasure_special {
    void destroy()
        { static_cast< derived * >( this )-> ~ derived(); }
    void move( void * dest ) &&
        { new (dest) derived( std::move( * static_cast< derived * >( this ) ) ); }
    void copy( void * dest ) const &
        { new (dest) derived( * static_cast< derived const * >( this ) ); }
};

// This accessor avoids instantiating erasure_special::copy for non-copyable types.
template< typename derived >
constexpr typename std::enable_if< std::is_copy_constructible< derived >::value >::type
( derived::* ( * erasure_copy ( derived const & ) ) ) ( void * ) const &
    { return & derived::erasure_special::copy; }
template< typename derived >
constexpr typename std::enable_if< ! std::is_copy_constructible< derived >::value >::type
( derived::* ( * erasure_copy ( derived const & ) ) ) ( void * ) const &
    { return nullptr; }

template< typename >
struct const_unsafe_case;

#define DISPATCH_BASE_CASE( NAME ) \
template< typename derived, std::size_t n, typename ... sig > \
struct NAME ## _dispatch { \
    static_assert ( sizeof ... (sig) == 0, "An unsupported function signature was detected." ); \
    void operator () () = delete; \
};

#define UNPACK(...) __VA_ARGS__
#define IGNORE(...)

#define DISPATCH_CASE( QUALS, UNSAFE, NAME, IMPL ) \
template< typename derived, std::size_t table_index, typename ret, typename ... args, typename ... sig > \
struct NAME ## _dispatch< derived, table_index, ret( args ... ) QUALS, sig ... > \
    : NAME ## _dispatch< derived, table_index+1, sig ... \
        UNSAFE (, const_unsafe_case< ret( args ... ) >) > { \
    using NAME ## _dispatch< derived, table_index+1, sig ... \
        UNSAFE (, const_unsafe_case< ret( args ... ) >) >::operator (); \
    ret operator () ( args ... a ) QUALS { UNPACK IMPL } \
};

#define DISPATCH_CQ( MACRO, UNSAFE, QUALS ) MACRO( QUALS, UNSAFE ) MACRO( const QUALS, UNSAFE )
#define DISPATCH_CV( MACRO, UNSAFE, QUALS ) DISPATCH_CQ( MACRO, UNSAFE, QUALS ) DISPATCH_CQ( MACRO, IGNORE, volatile QUALS )
#define DISPATCH_CVREFQ( MACRO, UNSAFE, QUALS ) \
    DISPATCH_CV( MACRO, UNSAFE, QUALS ) DISPATCH_CV( MACRO, IGNORE, & QUALS ) DISPATCH_CV( MACRO, IGNORE, && QUALS )
#define DISPATCH_ALL( MACRO ) DISPATCH_CVREFQ( MACRO, UNPACK, )

// A bogus base member is undefined, [expr.static.cast]/12.
// However, [expr.mptr.oper]/4 mentions dynamic type, which suggests that the overall usage is OK.
template< typename base, typename derived, typename t, typename actual_base >
t base::* ptm_cast( t actual_base::* ptm )
    { static_assert ( std::is_base_of< base, derived >::value, "Not a base class." );
    return static_cast< t base::* >( static_cast< t derived::* >( ptm ) ); }

#define DISPATCH_TABLE( NAME, TPARAM, TARG ) \
template< UNPACK TPARAM > \
typename erasure_base< sig ... >::dispatch_table const NAME< UNPACK TARG >::table = { \
    ptm_cast< erasure_base< sig ... >, NAME >( & NAME::destroy ), \
    ptm_cast< erasure_base< sig ... >, NAME >( & NAME::move ), \
    ptm_cast< erasure_base< sig ... >, NAME >( & NAME::copy ), \
    ptm_cast< erasure_base< sig ... >, NAME >( static_cast< sig NAME::* >( & NAME::operator () ) ) ... \
};

DISPATCH_BASE_CASE( null )
#define NULL_CASE( QUALS, UNSAFE ) DISPATCH_CASE( QUALS, IGNORE, null, ( throw std::bad_function_call{}; ) )
DISPATCH_ALL( NULL_CASE )
#undef NULL_CASE

template< typename ... sig >
struct null_erasure
    : erasure_base< sig ... >
    , erasure_special< null_erasure< sig ... > >
    , null_dispatch< null_erasure< sig ... >, 0, sig ... > {
    static const typename erasure_base< sig ... >::dispatch_table table;
    
    null_erasure() noexcept
        : null_erasure::erasure_base( table ) {}
};

DISPATCH_TABLE( null_erasure, ( typename ... sig ), ( sig ... ) )


template< typename t >
struct add_reference
    { typedef t & type; };
template< typename t >
struct add_reference< t && >
    { typedef t && type; };

DISPATCH_BASE_CASE( local )
#define LOCAL_CASE( QUALS, UNSAFE ) DISPATCH_CASE( QUALS, IGNORE, local, ( \
    return static_cast< typename add_reference< derived QUALS >::type >( * this ) \
        .target( std::forward< args >( a ) ... ); \
) )
DISPATCH_ALL( LOCAL_CASE )
#undef LOCAL_CASE

template< typename target_type, typename ... sig >
struct local_erasure
    : erasure_base< sig ... >
    , erasure_special< local_erasure< target_type, sig ... > >
    , local_dispatch< local_erasure< target_type, sig ... >, 0, sig ... > {
    static const typename erasure_base< sig ... >::dispatch_table table;
    
    target_type target;
    
    template< typename ... arg >
    local_erasure( arg && ... a )
        : local_erasure::erasure_base( table )
        , target( std::forward< arg >( a ) ... ) {}
};

DISPATCH_TABLE( local_erasure, ( typename target_type, typename ... sig ), ( target_type, sig ... ) )

// TODO: ptm_erasure using a decltype< mem_fn > target, allocator_erasure for the heap.

/*template< typename alloc, typename payload >
struct allocator_interface
    : private alloc {
    typedef alloc allocator_type;
    allocator_type get_allocator() const noexcept
        { return * static_cast< alloc const * >( this ); }
    allocator_type & allocator_ref() noexcept
        { return * static_cast< alloc * >( this ); }

    payload object;
    
    allocator_interface( alloc && in_alloc, payload && in_payload )
        : alloc( std::move( in_alloc ) ), payload( std::move( in_payload ) ) {}
    allocator_interface( alloc && in_alloc, payload const & in_payload )
        : alloc( std::move( in_alloc ) ), payload( in_payload ) {}
};*/

DISPATCH_BASE_CASE( wrapper )
#define WRAPPER_CASE( QUALS, UNSAFE ) DISPATCH_CASE( QUALS, UNSAFE, wrapper, ( \
    auto && self = static_cast< typename add_reference< derived QUALS >::type >( * this ); \
    boolalpha( std::cout ); \
    return ( std::forward< decltype (self) >( self ).erasure() \
            .* std::get< + dispatch_slot::base_index + table_index >( self.erasure().table ) ) \
        ( std::forward< args >( a ) ... ); \
) )
DISPATCH_ALL( WRAPPER_CASE )
#undef WRAPPER_CASE

template< typename derived, std::size_t n, typename ret, typename ... args, typename ... more >
struct wrapper_dispatch< derived, n, const_unsafe_case< ret( args ... ) >, more ... >
    : wrapper_dispatch< derived, n, more ... > {
    using wrapper_dispatch< derived, n, more ... >::operator ();
    [[deprecated( "It is unsafe to call a std::function of non-const signature through a const access path." )]]
    ret operator () ( args ... a ) const {
        return const_cast< derived & >( static_cast< derived const & >( * this ) )
            ( std::forward< args >( a ) ... );
    }
};

template< typename ... sig >
class wrapper_base
    : public wrapper_dispatch< wrapper_base< sig ... >, 0, sig ... > {
    typedef erasure_base< sig ... > erasure_xface;
    
    std::aligned_storage< sizeof (void *[4]) >::type storage;
    
    // These functions enter or recover from invalid states.
    // They get on the right side of [basic.life]/7.4, but mind the exceptions.
    
    void destroy() noexcept
        { ( erasure() .* std::get< + dispatch_slot::destructor >( erasure().table ) )(); }
    
    void init( std::nullptr_t = {} ) noexcept
        { new (& storage) null_erasure< sig ... >; }
    
    void init( wrapper_base && o ) noexcept {
        ( std::move( o ).erasure() .* std::get< + dispatch_slot::move_constructor >( o.erasure().table ) )( & storage );
        o = nullptr;
    }
    
    void init( wrapper_base const & o )
        { ( o.erasure() .* std::get< + dispatch_slot::copy_constructor >( o.erasure().table ) )( & storage ); }
    
    void init( unique_function< sig ... > const & o ) = delete;
    
    // Queries on potential targets.
    template< typename, typename = void >
    struct is_compatibly_wrapped : std::false_type {};
    template< typename source >
    struct is_compatibly_wrapped< source, typename std::enable_if<
            std::is_same< wrapper_base, typename std::decay< source >::type::wrapper_base >::value >::type >
        : std::true_type {};
    
    template< typename, typename = void >
    struct is_small : std::false_type {};
    template< typename source >
    struct is_small< source, typename std::enable_if<
            sizeof (local_erasure< source, sig ... >) <= sizeof (storage)
            && std::is_nothrow_move_constructible< source >::value >::type >
        : std::true_type {};
    
    template< typename source >
    struct is_local_adoption
        { static const bool value = ! is_compatibly_wrapped< source >::value && is_small< source >::value; };
    
    // Local erasures.
    template< typename source, typename ... arg
        , typename = typename std::enable_if< is_local_adoption< source >::value >::type >
    void init( any_piecewise_construct_tag< source >, arg && ... a )
        { new (& storage) local_erasure< source, sig ... >( std::forward< arg >( a ) ... ); }
    
    template< typename alloc, typename source, typename ... arg
        , typename = typename std::enable_if< is_local_adoption< source >::value >::type >
    void init( std::allocator_arg_t, alloc const &, any_piecewise_construct_tag< source > t, arg && ... a )
        { init( t, std::forward< arg >( a ) ... ); }
    
    // Adoption by copy/move is implemented in terms of in-place construction.
    template< typename source
        , typename = typename std::enable_if< ! is_compatibly_wrapped< source >::value >::type >
    void init( source && s )
        { init( any_piecewise_construct_tag< typename std::decay< source >::type >{}, std::forward< source >( s ) ); }
    
    template< typename alloc, typename source
        , typename = typename std::enable_if< ! is_compatibly_wrapped< source >::value >::type >
    void init( std::allocator_arg_t, alloc const &, source && s )
        { init( any_piecewise_construct_tag< typename std::decay< source >::type >{}, std::forward< source >( s ) ); }
    
public:
    erasure_xface & erasure() & { return reinterpret_cast< erasure_xface & >( storage ); }
    erasure_xface const & erasure() const & { return reinterpret_cast< erasure_xface const & >( storage ); }
    erasure_xface volatile & erasure() volatile & { return reinterpret_cast< erasure_xface volatile & >( storage ); }
    erasure_xface const volatile & erasure() const volatile & { return reinterpret_cast< erasure_xface const volatile & >( storage ); }
    erasure_xface && erasure() && { return reinterpret_cast< erasure_xface && >( storage ); }
    erasure_xface const && erasure() const && { return reinterpret_cast< erasure_xface const && >( storage ); }
    erasure_xface volatile && erasure() volatile && { return reinterpret_cast< erasure_xface volatile && >( storage ); }
    erasure_xface const volatile && erasure() const volatile && { return reinterpret_cast< erasure_xface const volatile && >( storage ); }
    
    template< typename ... arg >
    wrapper_base( arg && ... a )
        noexcept(noexcept( init( std::forward< arg >( a ) ... ) ))
        { init( std::forward< arg >( a ) ... ); }
    
    ~ wrapper_base() noexcept
        { destroy(); }
    
    template< typename source >
    wrapper_base & operator = ( source && s )
        noexcept( is_compatibly_wrapped< source >::value || is_small< source >::value )
    try {
        destroy();
        init( std::forward< source >( s ) );
        return * this;
    } catch (...) {
        init();
        throw;
    }
};

}

template< typename ... sig >
struct function
    : impl::wrapper_base< sig ... > {
    using function::wrapper_base::wrapper_base;
};


}

#endif
