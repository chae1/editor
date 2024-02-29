#include <iostream>
#include <vector>
#include <fmt/ranges.h>

using namespace std;

class A {
public:
    int i = 3;
    vector<int> v;
    void change_v() {	
	auto p = v.data();
	(*p) = 4;    
    }
};

int main() {
    A a;
    a.v.push_back(1);
    a.v.push_back(2);
    a.change_v();
    
    fmt::print("{}", fmt::join(a.v, " "));

    return 0;
}
