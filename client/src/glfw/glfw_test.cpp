#include "glfw_window.h"
#include "key_callback.h"

#include <exception>
#include <iostream>

using namespace glfw_window;
using namespace key_callback;

void key_callback2(GLFWwindow* window, int key, int scancode, int action, int mods) {
    if (action == GLFW_PRESS || action == GLFW_RELEASE) {
        std::cout << "key " << key << ", action " << action << "\n";
    }
}

int main(int argc, char* argv[]) {
    try {
        Glfw_window glfw_window { 500, 400, "editor" };
	glfw_window.set_key_callback(key_callback2);
        glfw_window.run_callback_loop();
    }

    catch (const std::exception& e) {
        std::cerr << e.what() << std::endl;
        return -1;
    }

    return 0;
}
