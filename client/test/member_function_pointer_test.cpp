#include <iostream>
#include <map>

struct A {
    A() {
        
    }

    void f(int i);

    typedef void (A::*mfp)(int);

    std::map<int, mfp> f_map = { {1, &A::f} };
};

void A::f(int i) {
    std::cout << i << std::endl;
}

int main() {
    A a;
    A::mfp fp = a.f_map[1];
    ((&a)->*fp)(100);

    return 0;
}
