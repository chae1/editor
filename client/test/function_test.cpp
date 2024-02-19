#include <iostream>
#include <vector>
#include <map>
#include <functional>
#include <any>

const std::vector<int> key = { 1, 2, 3 };
const std::map<int, std::vector<int>> keys = {{1, key}};

void func();
const std::map<int, std::function<void()>> funcs = {{1, func}};

void func() {
    std::cout << "f";
}

void func2(int key) {
    std::cout << key;
}

void func3(const auto& func) {
    func(1);
}

int main() {
    funcs.at(1)();
    func3(func2);
    

    int i = 1;
    auto func4 = [&]() {
	i = 2;
    };
    func4();
    std::cout << i;

    
}
