#include "cxx_function.hpp"
#include <cassert>

using namespace cxx_function;

struct vf {
    int operator () () { return m; }
    const int m;
};


int main() {
    function< int() > f = vf{ 3 }, g = vf{ 10 };
    assert ( f.target< vf >()->m == 3 );
    f.swap( g );
    assert ( f.target< vf >()->m == 10 );

    assert ( f() == 10 );
    assert ( g() == 3 );
}
