#include "cxx_function.hpp"
#include <cassert>
#include <array>

using namespace cxx_function;


int main() {
    std::array< int, 100 > d = { 1, 2, 3 };
    function< int *( int ) > f = [d] ( int i ) mutable { return d.data() + i; };
    assert ( f( 0 ) != d.data() );

    assert ( * f( 2 ) == 3 );
    auto g = f;
    * f( 2 ) = 5;
    assert ( * g( 2 ) == 3 );
    assert ( * f( 2 ) == 5 );
    auto p = f( 2 );
    g = std::move( f );
    assert ( g( 2 ) == p );
    try {
        f( 0 );
        abort();
    } catch ( std::bad_function_call & ) {}
}

