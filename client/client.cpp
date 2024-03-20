#include "src/vulkan/engine.h"
#include "src/socket/socket_client.h"

#include <glm/gtx/string_cast.hpp>
#include <iostream>
#include <stdexcept>
#include <cstdlib>
#include <string>
#include <sstream>

using namespace vk_engine;
using namespace socket_client;
using namespace glm;

void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods);
void framebufferResizeCallback(GLFWwindow* window, int width, int height);
void mouseButtonCallback(GLFWwindow* window, int button, int action, int mode);
void recreateSwapChainCallback();
void socket_listener();

int width = 500;
int height = 400;
int font_size = 50;

Engine engine { width, height, "3dit", key_callback, framebufferResizeCallback, mouseButtonCallback, recreateSwapChainCallback, socket_listener };

Socket_client& client = engine.client;

void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods) {
    if (action == GLFW_PRESS || action == GLFW_RELEASE) {
	client.send_msg("key-event");
	client.send_msg(to_string(key));
	client.send_msg(to_string(action));	
    }
}

void framebufferResizeCallback(GLFWwindow* window, int width, int height) {
    engine.framebufferResized = true;
}

void mouseButtonCallback(GLFWwindow* window, int button, int action, int mode) {
    if (button == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_PRESS) {
	engine.mouseLeftButtonPressed = true;
    } else if (button == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_RELEASE) {
	engine.mouseLeftButtonPressed = false;
    }

    // fmt::print("left buttom pressed : {}\n", engine.mouseLeftButtonPressed);
}

void recreateSwapChainCallback() {
    client.send_msg("resize-window");
    client.send_msg(to_string(width));
    client.send_msg(to_string(height));
}

std::string get_token(std::stringstream& ss) {
    std::string token;
    ss >> token;

    return token;
}

bool draw_flag = false;

void parse_msg() {
    std::stringstream ss(client.buf);
    std::string token = get_token(ss);
    
    if (token == "draw") {
	token = get_token(ss);

	if (token == "begin") {
	    engine.objs.clear();
	    
	    draw_flag = true;
	    
	} else if (token == "end") {
	    engine.updateStorageBuffer();
	    // engine.storageBufferUpdateFlag = true;

	    draw_flag = false;
	}
	
    } else {	
	if (draw_flag) {	
	    if (token == "char") {
		char c;
		float x, y, width, height;
		ss >> c >> x >> y >> width >> height;

		// fmt::print("{}, {}, {}, {}\n", x, y, width, height);
		// fmt::print("{}\n", to_string(vec3(translate(mat4(1.0f), vec3(x + width/2.0f, y + height/2.0f, 1.0f)) * scale(mat4(1.0f), vec3(width/2.0f, height/2.0f, 1.0f)) * vec4(vec3(-1.0f, -1.0f, 0.0f), 1))));
		
		StorageBufferObject ssbo;
		ssbo.model = glm::translate(glm::mat4(1.0f), glm::vec3(x + width/2.0f, y + height/2.0f, 1.0f)) * glm::scale(glm::mat4(1.0f), glm::vec3(width/2.0f, height/2.0f, 1.0f));
		ssbo.view = glm::mat4(1.0f);
		ssbo.proj = glm::mat4(1.0f);
		ssbo.color = glm::vec4(0.0f, 0.0f, 0.0f, 1.0f);
		ssbo.charId = engine.fontInfo.glyph_map[c];

		engine.objs.push_back(ssbo);
		
		// fmt::print("{}\n", ssbo.charId);
		
	    } else if (token == "cursor") {
		
	    }
	    
	} else {
	    if (token == "max-obj-num") {
		ss >> engine.maxSsboCount;
		engine.storageBufferRecreateFlag = true;
		
	    }
	}
    }
}

void socket_listener() {
    while (1) {
	client.read_msg();
	parse_msg();
    }
}

int main(int argc, char* argv[]) {
    try {
	if (argc != 3) {
            throw std::runtime_error("argc should be 3 (program ip:port username)");
        }
        std::string ip_port = argv[1];
	const char* username = argv[2];

	engine.init_font();
	engine.init_vulkan();

	client.init(ip_port);
	std::thread { socket_listener }.detach();
	
	client.send_msg("login");
	client.send_msg(username);
	client.send_msg(to_string(width));
	client.send_msg(to_string(height));
	client.send_msg(to_string(font_size));

        engine.run();
	
    } catch (const std::exception& e) {
        std::cerr << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
