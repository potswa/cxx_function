#include "cxx_function.hpp"

#include <cassert>
#include <map>
#include <scoped_allocator>
#include <string>

std::map< int, std::size_t > pool;

template< typename t >
struct pool_alloc : std::allocator< t > {
    int id;

    template< typename u >
    pool_alloc( pool_alloc< u > const & o )
        : id( o.id ) {}

    pool_alloc( int in_id )
        : id( in_id ) {}

    t * allocate( std::size_t n ) {
        n *= sizeof (t);
        pool[ id ] += n;
        return static_cast< t * >( ::operator new( n ) );
    }

    void deallocate( t * p, std::size_t n ) {
        n *= sizeof (t);
        pool[ id ] -= n;
        return ::operator delete( p );
    }

    template< typename o > struct rebind { typedef pool_alloc< o > other; };
    
    typedef std::false_type is_always_equal;
    typedef std::false_type propagate_on_container_move_assignment;
};

template< typename t, typename u >
bool operator == ( pool_alloc< t > lhs, pool_alloc< u > rhs )
    { return lhs.id == rhs.id; }
template< typename t, typename u >
bool operator != ( pool_alloc< t > lhs, pool_alloc< u > rhs )
    { return lhs.id != rhs.id; }

typedef std::basic_string< char, std::char_traits< char >, pool_alloc< char > > pool_string;

struct stateful_op {
    pool_string state;

    stateful_op( stateful_op const & o, pool_alloc< stateful_op > a )
        : state( o.state, a ) {}

    explicit stateful_op( pool_string s )
        : state( std::move( s ) ) {}

    void operator () () const
        { /* std::cout << "op says " << state << " from pool id " << state.get_allocator().id << '\n'; */ }
};

namespace std {
    template< typename a > struct uses_allocator< stateful_op, a > : std::true_type {};
}

using namespace cxx_function;

int main() {
    stateful_op op( { "hello from a very long string", pool_alloc< char >{ 0 } } );

    function_container< std::scoped_allocator_adaptor< pool_alloc<char> >, void() > fc1( std::allocator_arg, pool_alloc< char >{ 1 } );
    fc1 = op;
    function< void() > fv = fc1;
    function_container< std::scoped_allocator_adaptor< pool_alloc<char> >, void() > fc2( std::allocator_arg, pool_alloc< char >{ 2 } );
    fc2 = fv;
    
    op();
    fc1();
    fc2();
    fv();
    
    assert ( pool[ 0 ] == 32 ); // 32 bytes to store the 29-byte string
    assert ( pool[ 1 ] == 128 ); // 128 bytes for two 29-byte strings + two stateful_ops
    assert ( pool[ 2 ] == 64 ); // 64 bytes for one 29-byte string + one stateful_op
}
