#include "engine.h"
#include <iostream>
#include <stdexcept>
#include <cstdlib>
#include <string>

#include "../socket/socket_client.h"

using namespace vk_engine;
using namespace socket_client;

void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods);
void socket_listener();

Engine engine { 500, 400, "3dit", key_callback, socket_listener };
Socket_client& client = engine.client;

void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods) {
    if (action == GLFW_PRESS || action == GLFW_RELEASE) {
	client.send_msg("key-event");
	client.send_msg(to_string(key));
	client.send_msg(to_string(action));	
    }
}

void socket_listener() {
    while (1) {
	client.read_msg();
    }
}

int main(int argc, char* argv[]) {
    try {
	if (argc != 3) {
            throw std::runtime_error("argc should be 3 (progrm ip:port username)");
        }
        std::string ip_port = argv[1];
	client.init(ip_port);

	int width = 0, height = 0;
	glfwGetFramebufferSize(engine.window, &width, &height);
	
	client.send_msg("login");
	client.send_msg(argv[2]);
	client.send_msg(to_string(width));
	client.send_msg(to_string(height));
	
        engine.run();
	
    } catch (const std::exception& e) {
        std::cerr << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
