// cxx_function.hpp: major evolution for std::function
// Copyright 2015 by David Krauss.
// This source is released under the MIT license, http://opensource.org/licenses/MIT

#ifndef INCLUDED_CXX_FUNCTION_HPP
#define INCLUDED_CXX_FUNCTION_HPP

#include <exception>
#include <functional> // for std::bad_function_call and std::mem_fn
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
struct in_place_t {};

#if __cplusplus >= 201402
template< typename t >
in_place_t< t > in_place;
#endif

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
    target_access,
    target_type,
    allocator_type,
    
    base_index
};
constexpr int operator + ( dispatch_slot e ) { return static_cast< int >( e ); }

// "Abstract" base class for the island inside the wrapper class, e.g. std::function.
// This must appear first in the derived class layout.
template< typename ... sig >
struct erasure_base {
    typedef std::tuple<
        void (erasure_base::*)(), // destructor
        void (erasure_base::*)( void * dest, void * alloc ) &&, // move constructor
        void (erasure_base::*)( void * dest, void * alloc ) const &, // copy constructor
        
        void const * (erasure_base::*)() const, // target access
        std::type_info const &, // target_type
        std::type_info const *, // allocator_type
        
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
    void move( void * dest, void * ) &&
        { new (dest) derived( std::move( * static_cast< derived * >( this ) ) ); }
    void copy( void * dest, void * ) const &
        { new (dest) derived( * static_cast< derived const * >( this ) ); }
};

// These accessors avoid instantiating functions that do not exist or would be trivial.
template< typename derived >
constexpr typename std::enable_if<
    ! std::is_trivially_destructible< derived >::value >::type
( derived::* erasure_destroy() ) ()
    { return & derived::destroy; }
template< typename derived >
constexpr typename std::enable_if<
    std::is_trivially_destructible< derived >::value >::type
( derived::* erasure_destroy() ) ()
    { return nullptr; }

template< typename derived >
constexpr typename std::enable_if<
    ! std::is_trivially_constructible< derived, derived >::value >::type
( derived::* erasure_move() ) ( void *, void * ) &&
    { return & derived::move; }
template< typename derived >
constexpr typename std::enable_if<
    std::is_trivially_constructible< derived, derived >::value >::type
( derived::* erasure_move() ) ( void *, void * ) &&
    { return nullptr; }

template< typename derived >
constexpr typename std::enable_if<
    std::is_copy_constructible< derived >::value
    && ! std::is_trivially_copy_constructible< derived >::value >::type
( derived::* erasure_copy() ) ( void *, void * ) const &
    { return & derived::copy; }
template< typename derived >
constexpr typename std::enable_if<
    ! std::is_copy_constructible< derived >::value
    || std::is_trivially_copy_constructible< derived >::value >::type
( derived::* erasure_copy() ) ( void *, void * ) const &
    { return nullptr; }

template< typename, typename = void >
struct is_allocator_erasure : std::false_type {};
template< typename erasure >
struct is_allocator_erasure< erasure, decltype(void(typename std::allocator_traits< typename erasure::common_allocator >::pointer{})) >
    : std::true_type {};

template< typename derived >
constexpr typename std::enable_if< is_allocator_erasure< derived >::value,
std::type_info const * >::type erasure_allocator_type()
    { return & typeid (typename derived::common_allocator); }

template< typename derived >
constexpr typename std::enable_if< ! is_allocator_erasure< derived >::value,
std::type_info const * >::type erasure_allocator_type()
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

#define DISPATCH_CQ( MACRO, UNSAFE, QUALS ) MACRO( QUALS, UNSAFE ) MACRO( const QUALS, IGNORE )
#define DISPATCH_CV( MACRO, UNSAFE, QUALS ) DISPATCH_CQ( MACRO, UNSAFE, QUALS ) DISPATCH_CQ( MACRO, IGNORE, volatile QUALS )
#define DISPATCH_CVREFQ( MACRO, QUALS ) DISPATCH_CV( MACRO, IGNORE, & QUALS ) DISPATCH_CV( MACRO, IGNORE, && QUALS )
#define DISPATCH_ALL( MACRO ) DISPATCH_CV( MACRO, UNPACK, ) DISPATCH_CVREFQ( MACRO, )

// A bogus base member is undefined, [expr.static.cast]/12.
// However, [expr.mptr.oper]/4 mentions dynamic type, which suggests that the overall usage is OK.
template< typename base, typename derived, typename t, typename actual_base >
t base::* ptm_cast( t actual_base::* ptm )
    { return static_cast< t base::* >( static_cast< t derived::* >( ptm ) ); }

#define DISPATCH_TABLE( NAME, TARGET_TYPE, TPARAM, TARG ) \
template< UNPACK TPARAM > \
typename erasure_base< sig ... >::dispatch_table const NAME< UNPACK TARG >::table = { \
    ptm_cast< erasure_base< sig ... >, NAME >( erasure_destroy< NAME >() ), \
    ptm_cast< erasure_base< sig ... >, NAME >( erasure_move< NAME >() ), \
    ptm_cast< erasure_base< sig ... >, NAME >( erasure_copy< NAME >() ), \
    ptm_cast< erasure_base< sig ... >, NAME >( & NAME::target_access ), \
    typeid (TARGET_TYPE), \
    erasure_allocator_type< NAME >(), \
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
    
    void const * target_access() const { return nullptr; }
    
    null_erasure() noexcept
        : null_erasure::erasure_base( table ) {}
};

DISPATCH_TABLE( null_erasure, void, ( typename ... sig ), ( sig ... ) )


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
    
    void const * target_access() const { return & target; }
    
    template< typename ... arg >
    local_erasure( arg && ... a )
        : local_erasure::erasure_base( table )
        , target( std::forward< arg >( a ) ... ) {}
};

DISPATCH_TABLE( local_erasure, target_type, ( typename target_type, typename ... sig ), ( target_type, sig ... ) )

DISPATCH_BASE_CASE( ptm )
#define PTM_CASE( QUALS, UNSAFE ) DISPATCH_CASE( QUALS, IGNORE, ptm, ( \
    return std::mem_fn( static_cast< derived const volatile & >( * this ).target )( std::forward< args >( a ) ... ); \
) )
DISPATCH_ALL( PTM_CASE )
#undef PTM_CASE

template< typename target_type, typename ... sig >
struct ptm_erasure
    : erasure_base< sig ... >
    , erasure_special< ptm_erasure< target_type, sig ... > >
    , ptm_dispatch< ptm_erasure< target_type, sig ... >, 0, sig ... > {
    static const typename erasure_base< sig ... >::dispatch_table table;
    
    target_type target;
    
    void const * target_access() const { return & target; }
    
    ptm_erasure( target_type a )
        : ptm_erasure::erasure_base( table )
        , target( a ) {}
};

DISPATCH_TABLE( ptm_erasure, target_type, ( typename target_type, typename ... sig ), ( target_type, sig ... ) )


DISPATCH_BASE_CASE( allocator )
#define ALLOCATOR_CASE( QUALS, UNSAFE ) DISPATCH_CASE( QUALS, IGNORE, allocator, ( \
    return ( * static_cast< typename add_reference< decltype (std::declval< derived >().target) QUALS >::type >( \
            static_cast< typename add_reference< derived QUALS >::type >( * this ) \
        .target ) )( std::forward< args >( a ) ... ); \
) )
DISPATCH_ALL( ALLOCATOR_CASE )
#undef ALLOCATOR_CASE

template< typename allocator >
using common_allocator_rebind = typename std::allocator_traits< allocator >::template rebind_alloc< char >;

template< typename derived, typename v = void >
struct allocator_erasure_special
    : erasure_special< derived > {};

template< typename allocator, typename target_type, typename ... sig >
struct allocator_erasure
    : erasure_base< sig ... >
    , allocator
    , allocator_erasure_special< allocator_erasure< allocator, target_type, sig ... > >
    , allocator_dispatch< allocator_erasure< allocator, target_type, sig ... >, 0, sig ... > {
    using allocator_erasure::allocator_erasure_special::destroy;
    typedef std::allocator_traits< allocator > allocator_traits;
    typedef common_allocator_rebind< allocator > common_allocator;
    
    static const typename erasure_base< sig ... >::dispatch_table table;
    
    typename allocator_traits::pointer target;
    
    allocator & alloc() { return static_cast< allocator & >( * this ); }
    allocator const & alloc() const { return static_cast< allocator const & >( * this ); }
    target_type * target_address() { return std::addressof( * target ); }
    void const * target_access() const { return std::addressof( * target ); }
    
    template< typename ... arg >
    explicit allocator_erasure( allocator const & in_alloc, arg && ... a )
        : allocator_erasure::erasure_base( table )
        , allocator( in_alloc )
        , target( allocator_traits::allocate( alloc(), 1 ) )
        { allocator_traits::construct( alloc(), target_address(), std::forward< arg >( a ) ... ); }
    
    ~ allocator_erasure() {
        if ( target ) {
            allocator_traits::destroy( alloc(), target_address() );
            allocator_traits::deallocate( alloc(), target, 1 );
        }
    }
    allocator_erasure( allocator_erasure && o ) noexcept
        : allocator_erasure::erasure_base( table )
        , allocator( std::move( o.alloc() ) )
        , target( std::move( o.target ) )
        { o.target = nullptr; }
    
    allocator_erasure( std::allocator_arg_t, allocator const & dest_allocator, allocator_erasure && o )
        : allocator_erasure::erasure_base( table )
        , allocator( dest_allocator )
        , target( allocator_traits::allocate( alloc(), 1 ) )
        { allocator_traits::construct( alloc(), target_address(), std::move( * o.target ) ); }
    
    allocator_erasure( allocator_erasure const & o ) // This is only a placeholder for is_copy_constructible. It never gets called.
        : allocator_erasure( std::allocator_arg, o, o.alloc() ) {}
    
    allocator_erasure( std::allocator_arg_t, allocator & dest_allocator, allocator_erasure const & o )
        : allocator_erasure( std::allocator_arg, o, static_cast< allocator const & >( dest_allocator ) )
        { dest_allocator = alloc(); }
    
    allocator_erasure( std::allocator_arg_t, allocator const & dest_allocator, allocator_erasure const & o )
        : allocator_erasure::erasure_base( table )
        , allocator( dest_allocator )
        , target( allocator_traits::allocate( alloc(), 1 ) )
        { allocator_traits::construct( alloc(), target_address(), * o.target ); }
};

template< typename allocator, typename target_type, typename ... sig >
struct allocator_erasure_special< allocator_erasure< allocator, target_type, sig ... >,
    typename std::enable_if< ! std::allocator_traits< allocator >::propagate_on_container_move_assignment::value
        IGNORE( && ! std::allocator_traits< allocator >::is_always_equal::value ) >::type >
    : erasure_special< allocator_erasure< allocator, target_type, sig ... > > {
    typedef allocator_erasure< allocator, target_type, sig ... > derived;
    typedef common_allocator_rebind< allocator > common_allocator;
    allocator const & alloc() const { return static_cast< derived const & >( * this ); }
    
    void move( void * dest, void * dest_allocator_v ) && {
        auto dest_allocator = static_cast< common_allocator * >( dest_allocator_v );
        if ( ! dest_allocator || * dest_allocator == alloc() ) {
            new (dest) derived( static_cast< derived && >( * this ) );
        } else {
            new (dest) derived( std::allocator_arg, * dest_allocator, static_cast< derived && >( * this ) );
            * dest_allocator = alloc();
        }
    }
    void copy( void * dest, void * dest_allocator_v ) const & {
        auto dest_allocator_p = static_cast< common_allocator * >( dest_allocator_v );
        allocator const & dest_allocator = dest_allocator_p?
            static_cast< allocator const & >( * dest_allocator_p ) : alloc();
        auto & e = * new (dest) derived( std::allocator_arg, dest_allocator, static_cast< derived const & >( * this ) );
        if ( dest_allocator_p ) * dest_allocator_p = e.alloc();
    }
};

DISPATCH_TABLE( allocator_erasure, target_type, ( typename allocator, typename target_type, typename ... sig ), ( allocator, target_type, sig ... ) )


template< bool ... cond >
struct logical_intersection
    : std::true_type {};
template< bool ... cond >
struct logical_intersection< true, cond ... >
    : logical_intersection< cond ... >::type {};
template< bool ... cond >
struct logical_intersection< false, cond ... >
    : std::false_type {};

template< typename t, typename sig, typename = void >
struct is_callable : std::false_type {};

#define IS_CALLABLE_CASE( QUALS, UNSAFE ) \
template< typename t, typename ret, typename ... arg > \
struct is_callable< t, ret( arg ... ) QUALS, \
    typename std::enable_if< std::is_convertible< \
        typename std::result_of< typename add_reference< t QUALS >::type ( arg ... ) >::type \
    , ret >::value >::type > \
    : std::true_type {};

DISPATCH_ALL( IS_CALLABLE_CASE )
#undef IS_CALLABLE_CASE

template< typename sig >
struct is_callable< std::nullptr_t, sig >
    : std::true_type {};

template< typename ... sig >
struct is_all_callable {
    template< typename t >
    using temp = typename logical_intersection< is_callable< t, sig >::value ... >::type;
};

template< typename self, typename ... sig >
struct is_copyable_all_callable {
    template< typename t, typename = void >
    struct temp : std::integral_constant< bool,
        std::is_copy_constructible< t >::value
        && is_all_callable< sig ... >::template temp< t >::value > {};
    
    template< typename v > // Presume that self is a copyable wrapper, since that is what uses this metafunction.
    struct temp< self, v > : std::true_type {};
};

template< typename source >
struct is_noexcept_erasable : std::false_type {};
template<>
struct is_noexcept_erasable< std::nullptr_t > : std::true_type {};
template< typename t >
struct is_noexcept_erasable< t * > : std::true_type {};
template< typename t, typename c >
struct is_noexcept_erasable< t c::* > : std::true_type {};
template< typename t >
struct is_noexcept_erasable< std::reference_wrapper< t > > : std::true_type {};

DISPATCH_BASE_CASE( wrapper )
#define WRAPPER_CASE( QUALS, UNSAFE ) DISPATCH_CASE( QUALS, UNSAFE, wrapper, ( \
    auto && self = static_cast< typename add_reference< derived QUALS >::type >( * this ); \
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
    template< template< typename ... > class, typename, typename ... >
    friend class wrapper;
    typedef std::aligned_storage< sizeof (void *[3]) >::type effective_storage_type;
protected:
    std::aligned_storage< sizeof (void *[4]), alignof(effective_storage_type) >::type storage;
    void * storage_address() { return & storage; }
    
    // Queries on potential targets.
    template< typename, typename = void >
    struct is_small : std::false_type {};
    template< typename source >
    struct is_small< source, typename std::enable_if<
            sizeof (local_erasure< source, sig ... >) <= sizeof (storage)
            && alignof (source) <= alignof (decltype (storage))
            && std::is_nothrow_move_constructible< source >::value >::type >
        : std::true_type {};
    
    // These functions enter or recover from invalid states.
    // They get on the right side of [basic.life]/7.4, but mind the exceptions.
    
    void destroy() noexcept {
        auto nontrivial = std::get< + dispatch_slot::destructor >( erasure().table );
        if ( nontrivial ) ( erasure() .* nontrivial )();
    }
    
    // Default, move, and copy construction.
    void init( in_place_t< std::nullptr_t >, std::nullptr_t ) noexcept
        { new (storage_address()) null_erasure< sig ... >; }
    
    // Pointers are local callables.
    template< typename t >
    void init( in_place_t< t * >, t * p ) noexcept {
        if ( p ) new (storage_address()) local_erasure< t *, sig ... >( p );
        else init( in_place_t< std::nullptr_t >{}, nullptr );
    }
    // PTMs are like local callables.
    template< typename t, typename c >
    void init( in_place_t< t c::* >, t c::* ptm ) noexcept {
        if ( ptm ) new (storage_address()) ptm_erasure< t c::*, sig ... >( ptm );
        else init( in_place_t< std::nullptr_t >{}, nullptr );
    }
public:
    #define ERASURE_ACCESS( QUALS, UNSAFE ) \
        erasure_base< sig ... > QUALS erasure() QUALS { return reinterpret_cast< erasure_base< sig ... > QUALS >( storage ); }
    DISPATCH_CVREFQ( ERASURE_ACCESS, )
    #undef ERASURE_ACCESS
    
    std::type_info const & target_type() const noexcept
        { return std::get< + dispatch_slot::target_type >( erasure().table ); }
    
    template< typename want >
    want const * target() const noexcept {
        if ( typeid (want) != target_type() ) return nullptr;
        return static_cast< want const * >( ( erasure() .* std::get< + dispatch_slot::target_access >( erasure().table ) )() );
    }
    template< typename want >
    want * target() noexcept
        { return const_cast< want * >( static_cast< wrapper_base const & >( * this ).target< want >() ); }
    
    explicit operator bool () const noexcept
        { return target_type() != typeid (void); }
};

struct allocator_mismatch_error : std::exception
    { virtual char const * what() const noexcept override { return "An object could not be transferred into an incompatible memory allocation scheme."; } };

template< typename allocator >
class wrapper_allocator
    : common_allocator_rebind< allocator > {
    typedef std::allocator_traits< common_allocator_rebind< allocator > > allocator_traits;
protected:
    wrapper_allocator( wrapper_allocator && ) = default;
    wrapper_allocator( wrapper_allocator const & o )
        : common_allocator_rebind< allocator >( allocator_traits::select_on_container_copy_construction( o ) )
        {}
    
    wrapper_allocator & operator = ( wrapper_allocator && o )
        { if ( allocator_traits::propagate_on_container_move_assignment ) actual_allocator() = std::move( o.actual_allocator() ); }
    wrapper_allocator & operator = ( wrapper_allocator const & o )
        { if ( allocator_traits::propagate_on_container_copy_assignment ) actual_allocator() = o.actual_allocator(); }
    
    common_allocator_rebind< allocator > & actual_allocator()
        { return * this; }
    
    template< typename erasure_base >
    common_allocator_rebind< allocator > * compatible_allocator( erasure_base const & e ) {
        std::type_info const * type = std::get< + dispatch_slot::allocator_type >( e.table );
        if ( type == nullptr ) return nullptr;
        if ( * type != typeid (actual_allocator()) ) throw allocator_mismatch_error{};
        return & actual_allocator();
    }
public:
    explicit wrapper_allocator( allocator const & in_alloc = allocator{} ) noexcept
        : common_allocator_rebind< allocator >( in_alloc ) {}
    
    wrapper_allocator( std::allocator_arg_t, allocator const & in_alloc )
        : wrapper_allocator( in_alloc ) {}
    
    typedef allocator allocator_type;
    allocator get_allocator() const
        { return * this; }
};

class wrapper_no_allocator {
protected:
    std::allocator< char > actual_allocator() { return {}; }
    
    template< typename erasure_base >
    static constexpr void * compatible_allocator( erasure_base const & e )
        { return nullptr; }
    
    struct poor_conversion { template< typename t > poor_conversion( t const & ) {} };
public:
    wrapper_no_allocator() = default;
    wrapper_no_allocator( poor_conversion ) noexcept {}
    wrapper_no_allocator( std::allocator_arg_t, poor_conversion ) noexcept {}
};

template< template< typename ... > class is_targetable, typename allocator_manager, typename ... sig >
class wrapper
    : public allocator_manager
    , wrapper_base< sig ... > {
    //friend class wrapper_base< sig ... >;
    template< template< typename ... > class, typename, typename ... >
    friend class wrapper;
    
    using wrapper::wrapper_base::storage;
    using wrapper::wrapper_base::storage_address;
    using wrapper::wrapper_base::init;
    using wrapper::wrapper_base::destroy;
    
    template< typename, typename = void >
    struct is_compatibly_wrapped : std::false_type {};
    template< typename source >
    struct is_compatibly_wrapped< source, typename std::enable_if<
            std::is_same< typename wrapper::wrapper_base, typename source::wrapper_base >::value >::type >
        : std::true_type {};
    
    template< typename t >
    using is_small = typename wrapper::wrapper_base::template is_small< t >;
    
    template< typename source >
    typename std::enable_if< is_compatibly_wrapped< source >::value >::type
    init( in_place_t< source >, source && s ) noexcept {
        typename wrapper::wrapper_base & o = s;
        auto nontrivial = std::get< + dispatch_slot::move_constructor >( o.erasure().table );
        if ( ! nontrivial ) std::memcpy( storage_address(), & o.storage, sizeof (storage) );
        else ( std::move( o ).erasure() .* nontrivial )( storage_address(), allocator_manager::compatible_allocator( o.erasure() ) );
        o.destroy();
        o.init( in_place_t< std::nullptr_t >{}, nullptr );
    }
    template< typename source >
    typename std::enable_if< is_compatibly_wrapped< source >::value >::type
    init( in_place_t< source >, source const & s ) {
        static_assert ( std::is_copy_constructible< source >::value, "Allocator construction request bypassed unique_function copyability." );
        typename wrapper::wrapper_base const & o = s;
        auto nontrivial = std::get< + dispatch_slot::copy_constructor >( o.erasure().table );
        if ( ! nontrivial )std::memcpy( storage_address(), & o.storage, sizeof (storage) );
        else ( o.erasure() .* nontrivial )( storage_address(), allocator_manager::compatible_allocator( o.erasure() ) );
    }
    
    template< typename allocator, typename source, typename ... arg >
    typename std::enable_if< is_compatibly_wrapped< source >::value || is_small< source >::value >::type
    init( std::allocator_arg_t, allocator const &, in_place_t< source > t, arg && ... a )
        { init( t, std::forward< arg >( a ) ... ); }
    
    // Local erasures.
    template< typename source, typename ... arg >
    typename std::enable_if< is_small< source >::value >::type
    init( in_place_t< source >, arg && ... a )
        { new (storage_address()) local_erasure< source, sig ... >( std::forward< arg >( a ) ... ); }
    
    // Allocated erasures.
    template< typename in_allocator, typename source, typename ... arg >
    typename std::enable_if< ! is_compatibly_wrapped< source >::value && ! is_small< source >::value >::type
    init( std::allocator_arg_t, in_allocator && alloc, in_place_t< source >, arg && ... a ) {
        typedef typename std::allocator_traits< typename std::decay< in_allocator >::type >::template rebind_alloc< source > allocator;
        typedef allocator_erasure< allocator, source, sig ... > erasure;
        static_assert ( is_allocator_erasure< erasure >::value, "" );
        // TODO: Add a new erasure template to put the fancy pointer on the heap.
        static_assert ( sizeof (erasure) <= sizeof storage, "Stateful allocator or fancy pointer is too big for polymorphic function wrapper." );
        auto & e = * new (storage_address()) erasure( alloc, std::forward< arg >( a ) ... );
        allocator_manager::actual_allocator() = e.alloc();
    }
    template< typename source, typename ... arg >
    typename std::enable_if< ! is_compatibly_wrapped< source >::value && ! is_small< source >::value >::type
    init( in_place_t< source > t, arg && ... a )
        { init( std::allocator_arg, allocator_manager::actual_allocator(), t, std::forward< arg >( a ) ... ); }
    
public:
    using allocator_manager::allocator_manager;
    
    using wrapper::wrapper_base::operator ();
    using wrapper::wrapper_base::target;
    using wrapper::wrapper_base::target_type;
    using wrapper::wrapper_base::operator bool;
    
    wrapper() noexcept
        { init( in_place_t< std::nullptr_t >{}, nullptr ); }
    wrapper( wrapper && s ) noexcept
        { init( in_place_t< wrapper >{}, std::move( s ) ); }
    wrapper( wrapper const & s )
        { init( in_place_t< wrapper >{}, s ); }
    
    template< typename source,
        typename std::enable_if<
            is_targetable< typename std::decay< source >::type >::value
            && std::is_constructible< typename std::decay< source >::type, source >::value
        >::type * = nullptr >
    wrapper( source && s )
    noexcept( is_noexcept_erasable< typename std::decay< source >::type >::value || is_compatibly_wrapped< source >::value )
        : allocator_manager( std::forward< source >( s ) )
        { init( in_place_t< typename std::decay< source >::type >{}, std::forward< source >( s ) ); }

    // Prevent slicing fallback to copy/move constructor.
    template< typename source,
        typename std::enable_if<
            ! is_targetable< typename std::decay< source >::type >::value
            || ! std::is_constructible< typename std::decay< source >::type, source >::value
        >::type * = nullptr >
    wrapper( source && s ) = delete;
    
    template< typename allocator, typename source,
        typename = typename std::enable_if<
            is_targetable< typename std::decay< source >::type >::value
        >::type >
    wrapper( std::allocator_arg_t, allocator const & alloc, source && s )
    noexcept( is_noexcept_erasable< typename std::decay< source >::type >::value || is_compatibly_wrapped< source >::value )
        : allocator_manager( std::allocator_arg, alloc )
        { init( std::allocator_arg, alloc, in_place_t< typename std::decay< source >::type >{}, std::forward< source >( s ) ); }
    
    template< typename source, typename ... arg,
        typename = typename std::enable_if<
            is_targetable< source >::value
            && std::is_constructible< source, arg ... >::value
        >::type >
    wrapper( in_place_t< source > t, arg && ... a )
    noexcept( is_noexcept_erasable< source >::value
        || ( is_compatibly_wrapped< source >::value && std::is_nothrow_constructible< source, arg ... >::value ) )
        { init( t, std::forward< arg >( a ) ... ); }
    
    template< typename allocator, typename source, typename ... arg,
        typename = typename std::enable_if<
            is_targetable< source >::value
        >::type >
    wrapper( std::allocator_arg_t, allocator const & alloc, in_place_t< source > t, arg && ... a )
    noexcept( is_noexcept_erasable< source >::value
        || ( is_compatibly_wrapped< source >::value && std::is_nothrow_constructible< source, arg ... >::value ) )
        : allocator_manager( std::allocator_arg, alloc )
        { init( std::allocator_arg, alloc, t, std::forward< arg >( a ) ... ); }
    
    ~ wrapper() noexcept
        { destroy(); }
    
    template< typename source,
        typename = typename std::enable_if<
            is_targetable< typename std::decay< source >::type >::value
            && std::is_constructible< typename std::decay< source >::type, source >::value
        >::type >
    wrapper &
    operator = ( source && s )
    noexcept( is_noexcept_erasable< typename std::decay< source >::type >::value || is_compatibly_wrapped< source >::value ) {
        wrapper next( std::allocator_arg, this->get_allocator(), std::forward< source >( s ) );
        
        destroy();
        init( in_place_t< wrapper >{}, std::move( next ) );
        this->actual_allocator() = next.actual_allocator();
        return * this;
    }
    
    template< typename allocator, typename source,
        typename = typename std::enable_if<
            is_targetable< typename std::decay< source >::type >::value
        >::type >
    wrapper &
    assign( source && s, allocator const & alloc )
    noexcept( is_noexcept_erasable< typename std::decay< source >::type >::value || is_compatibly_wrapped< source >::value ) {
        wrapper next( std::allocator_arg, alloc, std::forward< source >( s ) );
        
        destroy();
        init( in_place_t< wrapper >{}, std::move( next ) );
        this->actual_allocator() = next.actual_allocator();
        return * this;
    }
    
    template< typename source, typename ... arg,
        typename = typename std::enable_if<
            is_targetable< source >::value
            && std::is_constructible< source, arg ... >::value
        >::type >
    wrapper &
    emplace_assign( arg && ... a )
    noexcept( is_noexcept_erasable< source >::value
        || ( is_compatibly_wrapped< source >::value && std::is_nothrow_constructible< source, arg ... >::value ) ) {
        wrapper next( std::allocator_arg, this->get_allocator(), in_place_t< source >{}, std::forward< arg >( a ) ... );
        
        destroy();
        init( in_place_t< wrapper >{}, std::move( next ) );
        this->actual_allocator() = next.actual_allocator();
        return * this;
    }
    
    template< typename source, typename allocator, typename ... arg,
        typename = typename std::enable_if<
            is_targetable< source >::value
        >::type >
    wrapper &
    allocate_assign( allocator const & alloc, arg && ... a )
    noexcept( is_noexcept_erasable< source >::value
        || ( is_compatibly_wrapped< source >::value && std::is_nothrow_constructible< source, arg ... >::value ) ) {
        wrapper next( std::allocator_arg, alloc, in_place_t< source >{}, std::forward< arg >( a ) ... );
        
        destroy();
        init( in_place_t< wrapper >{}, std::move( next ) );
        this->actual_allocator() = next.actual_allocator();
        return * this;
    }

    void swap( wrapper & other )
        { std::swap( * this, other ); }
};

}

template< typename ... sig >
class function
    : impl::wrapper< impl::is_copyable_all_callable< function< sig ... >, sig ... >::template temp, impl::wrapper_no_allocator, sig ... > {
    friend typename function::wrapper;
public:
    using function::wrapper::wrapper;
    
    using function::wrapper::operator ();
    using function::wrapper::operator =;
    function & operator = ( function const & o ) // Investigate why this is needed. Compiler bug?
        { return static_cast< typename function::wrapper & >( * this ) = o; }
    using function::wrapper::swap;
    using function::wrapper::target;
    using function::wrapper::target_type;
    using function::wrapper::operator bool;
    
    using function::wrapper::assign;
    using function::wrapper::emplace_assign;
    using function::wrapper::allocate_assign;
};

template< typename ... sig >
class unique_function
    : impl::wrapper< impl::is_all_callable< sig ... >::template temp, impl::wrapper_no_allocator, sig ... > {
    friend typename unique_function::wrapper;
public:
    using unique_function::wrapper::wrapper;
    
    unique_function() = default;
    unique_function( unique_function && ) = default;
    unique_function( unique_function const & ) = delete;
    
    using unique_function::wrapper::operator ();
    using unique_function::wrapper::operator =;
    using unique_function::wrapper::swap;
    using unique_function::wrapper::target;
    using unique_function::wrapper::target_type;
    using unique_function::wrapper::operator bool;
    
    using unique_function::wrapper::assign;
    using unique_function::wrapper::emplace_assign;
    using unique_function::wrapper::allocate_assign;
};

template< typename allocator, typename ... sig >
class function_container
    : impl::wrapper< impl::is_copyable_all_callable< function< sig ... >, sig ... >::template temp, impl::wrapper_allocator< allocator >, sig ... > {
    friend typename function_container::wrapper;
public:
    using function_container::wrapper::wrapper;
    
    using function_container::wrapper::operator ();
    using function_container::wrapper::operator =;
    function_container & operator = ( function_container const & o ) // Investigate why this is needed. Compiler bug?
        { return static_cast< typename function_container::wrapper & >( * this ) = o; }
    using function_container::wrapper::swap;
    using function_container::wrapper::target;
    using function_container::wrapper::target_type;
    using function_container::wrapper::operator bool;
    
    using function_container::wrapper::get_allocator;
    using function_container::wrapper::emplace_assign;
};

template< typename allocator, typename ... sig >
class unique_function_container
    : impl::wrapper< impl::is_all_callable< sig ... >::template temp, impl::wrapper_allocator< allocator >, sig ... > {
    friend typename unique_function_container::wrapper;
public:
    using unique_function_container::wrapper::wrapper;
    
    unique_function_container() = default;
    unique_function_container( unique_function_container && ) = default;
    unique_function_container( unique_function_container const & ) = delete;
    
    using unique_function_container::wrapper::operator ();
    using unique_function_container::wrapper::operator =;
    using unique_function_container::wrapper::swap;
    using unique_function_container::wrapper::target;
    using unique_function_container::wrapper::target_type;
    using unique_function_container::wrapper::operator bool;
    
    using unique_function_container::wrapper::get_allocator;
    using unique_function_container::wrapper::emplace_assign;
};

#define DEFINE_WRAPPER_OPS( NAME ) \
template< typename ... sig > \
bool operator == ( NAME< sig ... > const & a, std::nullptr_t ) \
    { return !a; } \
template< typename ... sig > \
bool operator != ( NAME< sig ... > const & a, std::nullptr_t ) \
    { return a; } \
template< typename ... sig > \
bool operator == ( std::nullptr_t, NAME< sig ... > const & a ) \
    { return !a; } \
template< typename ... sig > \
bool operator != ( std::nullptr_t, NAME< sig ... > const & a ) \
    { return a; }

DEFINE_WRAPPER_OPS( function )
DEFINE_WRAPPER_OPS( unique_function )
DEFINE_WRAPPER_OPS( function_container )
DEFINE_WRAPPER_OPS( unique_function_container )

}

#endif
