#include "cxx_function.hpp"
using namespace cxx_function;


void f( int ) {}
void f() {}

function< void( int ) > q = & f;

int main() {
    q = f;
}
