#include "glfw/glfw_window.h"
#include "socket/socket_client.h"
#include <exception>
#include <stdexcept>
#include <iostream>
#include <string>
#include <memory>

using namespace glfw_window;
using namespace socket_client;

Glfw_window* pglfw_window;
Socket_client* psocket_client;

constexpr const char* login_msg_begin = "0\n";
constexpr const char* key_event_msg_begin = "1\n";

inline void send_login_msg(const char* username) {
    psocket_client->write_msg(login_msg_begin);
    psocket_client->write_msg((std::string(username) + "\n").c_str());
}

inline void send_key_input_msg(int key, int action) {
    std::cout << "key " << key << ", action " << action << "\n";
    psocket_client->write_msg(key_event_msg_begin);
    psocket_client->write_msg((std::to_string(key) + "\n").c_str());
    psocket_client->write_msg((std::to_string(action) + "\n").c_str());
}

void callback(GLFWwindow* glfw_window, int key, int scancode, int action, int mods) {
    if (action == GLFW_PRESS || action == GLFW_RELEASE) {
        std::cout << "key " << key << ", action " << action << "\n";
	send_key_input_msg(key, action);
    }
}

int main(int argc, char* argv[]) {
    try {
	if (argc != 3) {
            throw std::runtime_error("error : argc != 3");
        }

	std::string ip_port = argv[1];
        char* username = argv[2];

	psocket_client = new Socket_client{ ip_port };
	send_login_msg(username);
	psocket_client->read_msg();
	
	pglfw_window = new Glfw_window{ 500, 400, "editor" };
	pglfw_window->set_key_callback(callback);
        pglfw_window->run_callback_loop();

	// 메롱ㅎㅎ
    }

    catch (const std::exception& e){
	std::cerr << e.what() << std::endl;
        return -1;
    }

    return 0;
}
