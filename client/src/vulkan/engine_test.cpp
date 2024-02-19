#include "engine.h"
#include <iostream>
#include <stdexcept>
#include <cstdlib>

using namespace glfw_window;
using namespace engine;

void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods) {
    if (action == GLFW_PRESS || action == GLFW_RELEASE) {
        std::cout << "key " << key << ", action " << action << "\n";
    }
}

int main() {
    Engine engine { 500, 400, "3edit", key_callback };
    
    try {
        engine.run();
    } catch (const std::exception& e) {
        std::cerr << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
