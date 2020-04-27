#include "cxx_function.hpp"

#include <cassert>

using namespace cxx_function;

int q = 0;

struct fs {
    void operator () () { q = 1; }
    void operator () () volatile { ++ q; }
};

int main() {
    function< void(), void() volatile > f = fs{};
    auto volatile g = f;
    
    f();
    assert ( q == 1 );
    g();
    assert ( q == 2 );
    
    #if __cplusplus >= 201402
    recover< fs >( g ) ();
    assert ( q == 3 );
    #endif
}
