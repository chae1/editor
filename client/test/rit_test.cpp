#include <iostream>
#include <vector>
#include <iterator>

void print_v(const std::vector<int>& v) {
    for (const auto& el : v) {
        std::cout << el << " ";
    }
    std::cout << "\n";
}

int main() {
    std::vector<int> v = { 1, 2, 3, 4, 5 };

    for (auto rit = v.rbegin(); rit != v.rend(); rit++) {
        int d = std::distance(v.begin(), rit.base());
        if (d == 2) break;
        else if (*rit == 3) {
            v.pop_back();
            break;
        }
        else v.pop_back();
    }

    print_v(v);
}
