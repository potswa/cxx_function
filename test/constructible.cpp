#include "cxx_function.hpp"
#include <type_traits>
#include <cassert>

using namespace cxx_function;

struct f {
    int operator () ( int ) & { return 0; }
};

static_assert ( std::is_default_constructible< function< int( int ) > >::value, "" );
static_assert ( std::is_default_constructible< unique_function< int( int ) > >::value, "" );

static_assert ( std::is_convertible< f, function< int( int ) > >::value, "" );
static_assert ( std::is_convertible< f, function< int( std::integral_constant< long, 100 > ) > >::value, "" );
static_assert ( ! std::is_convertible< f, function< int( int ) && > >::value, "" );
static_assert ( ! std::is_convertible< f, function< int( int ) &, int( int ) && > >::value, "" );

static_assert ( std::is_nothrow_constructible< function< int( int ) >, function< int( int ) > >::value, "" );
static_assert ( std::is_nothrow_constructible< function< int( int ) >, function< int( int ) > && >::value, "" );
static_assert ( ! std::is_nothrow_constructible< function< int( int ) >, function< int( int ) & > >::value, "" );
static_assert ( ! std::is_nothrow_constructible< function< int( int ) >, function< int( int ) const & > >::value, "" );

static_assert ( std::is_nothrow_constructible< function< int( int ) >, std::allocator_arg_t, std::allocator<void>,
    in_place_t< std::nullptr_t >, std::nullptr_t >::value, "" );
static_assert ( ! std::is_nothrow_constructible< function< int( int ) >, std::allocator_arg_t, std::allocator<void>,
    in_place_t< function< int( int ) > >, f >::value, "" );

typedef unique_function< int( int ) & > uft;
static_assert ( std::is_assignable< uft, uft >::value, "" );
static_assert ( std::is_assignable< uft, uft && >::value, "" );
static_assert ( ! std::is_assignable< uft, uft & >::value, "" );
static_assert ( ! std::is_assignable< uft, uft const & >::value, "" );
static_assert ( ! std::is_assignable< uft, uft const && >::value, "" );

typedef function< int( int ) & > ft;
static_assert ( std::is_assignable< ft, ft >::value, "" );
static_assert ( std::is_assignable< ft, ft && >::value, "" );
static_assert ( std::is_assignable< ft, ft & >::value, "" );
static_assert ( std::is_assignable< ft, ft const & >::value, "" );
static_assert ( std::is_assignable< ft, ft const && >::value, "" );

static_assert ( ! std::is_constructible< function< int( int ) & >, std::allocator_arg_t, std::allocator< uft >, uft >::value, "" );
static_assert ( ! std::is_constructible< function< int( int ) & >, std::allocator_arg_t, std::allocator< uft >, uft & >::value, "" );

static_assert ( ! std::is_constructible< function< int( int ) && >, f >::value, "" );

//uft a, q( std::allocator_arg, std::allocator< uft >{}, a ); // Fails.

int main () {}
