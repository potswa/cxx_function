#include "cxx_function.hpp"
#include <cassert>
using namespace cxx_function;

struct c {
    void operator() ( int ) {}
    operator function< void() > () const { return {}; }
};

int main() {
    function< void( int ) > a;
    function< void( long ) > b = a;
    assert ( ! b );
    b = c{};
    assert ( b );
}

