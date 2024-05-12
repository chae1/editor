#pragma once

#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>

namespace glfw_window {
    class Glfw_window {
    public:
        Glfw_window() = delete;
        Glfw_window(int width, int height, const char* title) : title { title } {
            glfwInit();
            window = glfwCreateWindow(width, height, title, NULL, NULL);
        }

        ~Glfw_window() {
            glfwDestroyWindow(window);
            glfwTerminate();
        }

	const char* title;
        GLFWwindow* window;
	
        void set_key_callback(void (*func)(GLFWwindow* window, int key, int scancode, int action, int mods));
        void run_callback_loop();
    };
}
