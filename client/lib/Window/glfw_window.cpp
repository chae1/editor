#include "glfw_window.h"

using namespace glfw_window;

void Glfw_window::set_key_callback(void (*func)(GLFWwindow* window, int key, int scancode, int action, int mods)) {
    glfwSetKeyCallback(window, func);
}

void Glfw_window::run_callback_loop() {
    while (!glfwWindowShouldClose(window)) {
        // glfwPollEvents();
    }
}
