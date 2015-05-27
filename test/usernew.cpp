#include "cxx_function.hpp"
#include <cassert>

bool intercepted = false;

template< typename t >
void * operator new( std::size_t n, t * p ) {
    intercepted = true;
    return operator new( n, (void*) p );
}

int main() {
    int arr[ 100 ];

    cxx_function::function< void() > q = [arr] () mutable { ++ arr[ 0 ]; };
    assert ( ! intercepted );
}

