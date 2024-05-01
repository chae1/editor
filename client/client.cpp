#include "engine.h"
#include "socket_client.h"

#include <glm/gtx/string_cast.hpp>

#include <iostream>
#include <stdexcept>
#include <cstdlib>
#include <string>
#include <sstream>
#include <thread>

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
int font_size = 40;

Engine engine { width, height, "3d editor", key_callback, framebufferResizeCallback, mouseButtonCallback, recreateSwapChainCallback, socket_listener };

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

// listening thread alone will call this function
void parse_msg_and_run_command() {
    std::stringstream ss(client.buf);
    std::string token = get_token(ss);
    
    if (token == "draw") {
	token = get_token(ss);

	if (token == "begin") {
	    engine.render_objs_mutex.lock();
	    engine.render_objs.clear();	    
	    draw_flag = true;
	    
	} else if (token == "end") {
	    // engine.updateStorageBuffer();
	    engine.storageBufferUpdateFlag = true;
	    engine.render_objs_mutex.unlock();
	    draw_flag = false;
	}
    } else {	
	if (draw_flag) {
	    if (token == "char") {
		char c;
		float char_left, char_up, char_width, char_height;
		ss >> c >> char_left >> char_up >> char_width >> char_height;

		// fmt::print("{}, {}, {}, {}\n", char_left, char_up, char_width, char_height);
		// fmt::print("{}\n", to_string(vec3(translate(mat4(1.0f), vec3(char_left + char_width/2.0f, char_up + char_height/2.0f, 1.0f)) * scale(mat4(1.0f), vec3(char_width/2.0f, char_height/2.0f, 1.0f)) * vec4(vec3(-1.0f, -1.0f, 0.0f), 1))));
		
		StorageBufferObject ssbo;
		ssbo.model = glm::translate(glm::mat4(1.0f), glm::vec3(char_left, char_up, 0.1f)) * glm::scale(glm::mat4(1.0f), glm::vec3(char_width/2.0f, char_height/2.0f, 0.1f));
		ssbo.view = glm::mat4(1.0f);
		ssbo.proj = glm::mat4(1.0f);
		ssbo.color = glm::vec4(0.0f, 0.0f, 0.0f, 1.0f);
		ssbo.charId = engine.fontInfo.glyph_map[c];

		engine.render_objs.push_back(ssbo);
		
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
	parse_msg_and_run_command();
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
