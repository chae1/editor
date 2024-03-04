#include <fmt/format.h>
#include <vector>
#include <algorithm>
#include <string>

using namespace std;

int main() {
    vector<const char*> v { "abc" };
    string s = "abc";
    if (find(v.begin(), v.end(), s) != v.end()) {
	fmt::print("{} found\n", s);
    }

    return 0;
}
