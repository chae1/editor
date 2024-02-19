#include <editor.hpp>
#include <chrono>
#include <unordered_map>

using namespace std::chrono_literals;

namespace edit {
    Editor::Editor(int width, int height, const char* name) {
        glfwInit();
        window = glfwCreateWindow(width, height, name, NULL, NULL);
        glfwSetKeyCallback(window, key_callback);
    }

    Editor::~Editor() {
        glfwDestroyWindow(window);
        glfwTerminate();
    }

    constexpr int repeat_rate_start_ms{400};
    constexpr int repeat_rate_repeat_ms{7};
    std::atomic<int> repeat_rate_ms;

    int current_key;

    void loop () {

    }

    void Editor::key_callback(GLFWwindow* window, int key, int scancode, int action, int mods) {
        if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) {
            glfwSetWindowShouldClose(window, GLFW_TRUE);
        }



        exec function_map[key]


    }
}
