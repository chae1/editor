#include "engine.h"
#include <iostream>
#include <stdexcept>
#include <cstdlib>

using namespace engine;
 
void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods) {
    if (action == GLFW_PRESS || action == GLFW_RELEASE) {
        std::cout << "key " << key << ", action " << action << std::endl;
    }
}

int main() {
    Engine engine { 500, 400, "3d-edit", key_callback };
    
    try {
        engine.run();
    } catch (const std::exception& e) {
        std::cerr << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
 
