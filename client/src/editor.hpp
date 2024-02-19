#pragma once

#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>

#include <vk/renderer.hpp>
#include <sock/client.hpp>

namespace edit {
    class Editor {
    public:
        Editor() = delete;
        Editor(int width, int height, const char* name);
        ~Editor();

        void connect_to_server(std::string ip_port);

    private:
        GLFWwindow* window;
        sock::Client client;
        vk::Renderer renderer(window);

        void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods);
    };

}
