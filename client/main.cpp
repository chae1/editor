#include <string>
#include <editor.hpp>

int main(int argc, char *argv[]) {
    std::string server_ip_port = argv[1];

    Editor editor;
    editor.init_key_callback();
    editor.init_connection(server_ip_port);
    editor.run_client_loop();
}
