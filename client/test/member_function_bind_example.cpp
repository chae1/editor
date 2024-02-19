#include <iostream>
#include <functional>

using namespace std::placeholders;

class A {
public:
    void func(int i1, int i2) {
        std::cout << i1 << i2 << "\n";
    }
};

void callback_exec(void (*callback)(int, int)) {
    callback(1, 2);
}

int main() {
    A a;
    std::function<void(int, int)> f1 = std::bind(&A::func, a, _1, _2);
    f1(1, 2);
    auto f2 = [=](int i1, int i2) {
	f1(i1, i2);
    };
    
    // callback_exec(f2);
}
