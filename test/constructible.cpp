#include <iostream>

#include "cxx_function.hpp"
#include <type_traits>
#include <cassert>

using namespace cxx_function;

struct f {
    int operator () ( int ) & { return 0; }
};


static_assert ( std::is_convertible< f, function< int( int ) > >::value, "" );
static_assert ( std::is_convertible< f, function< int( std::integral_constant< long, 100 > ) > >::value, "" );
static_assert ( ! std::is_convertible< f, function< int( int ) && > >::value, "" );
static_assert ( ! std::is_convertible< f, function< int( int ) &, int( int ) && > >::value, "" );

static_assert ( std::is_nothrow_constructible< function< int( int ) >, function< int( int ) > >::value, "" );
static_assert ( std::is_nothrow_constructible< function< int( int ) >, function< int( int ) > && >::value, "" );
static_assert ( ! std::is_nothrow_constructible< function< int( int ) >, function< int( int ) & > >::value, "" );
static_assert ( ! std::is_nothrow_constructible< function< int( int ) >, function< int( int ) const & > >::value, "" );

static_assert ( std::is_nothrow_constructible< function< int( int ) >, std::allocator_arg_t, std::allocator<void>, any_piecewise_construct_tag< std::nullptr_t >, std::nullptr_t >::value, "" );
static_assert ( std::is_nothrow_constructible< function< int( int ) >, std::allocator_arg_t, std::allocator<void>,
    any_piecewise_construct_tag< function< int( int ) > >, std::nullptr_t >::value, "" );
static_assert ( ! std::is_nothrow_constructible< function< int( int ) >, std::allocator_arg_t, std::allocator<void>,
    any_piecewise_construct_tag< function< int( int ) > >, f >::value, "" );

int main () {}
