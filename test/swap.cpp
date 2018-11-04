#include "cxx_function.hpp"
#include <cassert>

using namespace cxx_function;

int n = 0, c = 0;

struct vf {
    vf( int in )
        : m{ in }
        { ++ n; }
    vf( vf const & o )
        : m{ o.m }
        { ++ n; ++ c; }
    ~ vf() { -- n; }

    int operator () () { return m; }
    int m;
};


int main() {
    {
        function< int() > f = vf{ 3 }, g = vf{ 10 };
        std::swap( f, g );

        assert ( f() == 10 );
        assert ( g() == 3 );
    }
    assert ( n == 0 );
    assert ( c == 2 );
}
