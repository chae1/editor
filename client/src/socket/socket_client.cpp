#include "socket_client.h"
#include <regex>
#include <cstring>
#include <iostream>
#include <cerrno>

#include <windows.h>
#include <winerror.h>
#include <winsock2.h>
#include <ws2tcpip.h>

#include <fmt/format.h>

using namespace socket_client;

void Socket_client::init(const std::string& ip_port) {
    // Extract ip and port
    std::regex regex("([0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}):([0-9]{1,5})");
    std::smatch match;
    if (std::regex_match(ip_port, match, regex)) {
        server_ip = match[1].str();
        server_port = match[2].str();
    } else {
        throw std::runtime_error("run program as ./client xxx.xxx.xxx.xxx:20741");
    }

    WSADATA wsaData;
    int result = WSAStartup(MAKEWORD(2, 2), &wsaData);
    if (result != 0) {
        std::cerr << "WSAStartup failed with error: " << result << std::endl;
        throw std::runtime_error("WSAStartup failed");
    }

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_family = AF_INET;
    int ret = getaddrinfo(server_ip.c_str(), server_port.c_str(), &hints, &res);
    if (ret != 0) {
        std::cout << "WSAGetLastError() = " << WSAGetLastError() << "\n";
        throw std::runtime_error("getaddrinfo failed");
    }

    bool connect_succeed = false;
    for (rp = res; rp != NULL; rp = rp->ai_next) {
        sock = socket(AF_INET, SOCK_STREAM, 0);
        if (sock == INVALID_SOCKET)
            continue;

        // Set socket to non-blocking mode
        // int flags = fcntl(socket_fd, F_GETFL, 0);
        // if (flags == -1) {
        //     throw std::runtime_error("Error getting socket flags\n");
        // }
        // if (fcntl(socket_fd, F_SETFL, flags | O_NONBLOCK) == -1) {
        //     throw std::runtime_error("Error setting socket to non-blocking mode\n");
        // }
	
        if (connect(sock, rp->ai_addr, rp->ai_addrlen) != -1) {
            connect_succeed = true;
            break;
        }

        int error = WSAGetLastError();
        // Wait for connection to complete
        if (error == WSAEWOULDBLOCK) {
            fd_set writefds;
            FD_ZERO(&writefds);
            FD_SET(sock, &writefds);
        
            if (select(sock + 1, NULL, &writefds, NULL, NULL) < 0) {
                std::runtime_error("Error in select\n");
            }

            if (FD_ISSET(sock, &writefds)) {
                int so_error;
                socklen_t len = sizeof so_error;
                getsockopt(sock, SOL_SOCKET, SO_ERROR, reinterpret_cast<char*>(&so_error), &len);
                if (so_error != 0) {
                    std::runtime_error("Error connecting to server\n");
                }

                connect_succeed = true;
                break;
            }
        }

        closesocket(sock);
    }
    
    freeaddrinfo(res);

    if (!connect_succeed)
        throw std::runtime_error("connect failed");
}

Socket_client::Socket_client(const std::string& ip_port) {
    init(ip_port);
}

Socket_client::~Socket_client() {
    closesocket(sock);
}

void Socket_client::send_msg(const char *msg) {
    int ret = send(sock, msg, strlen(msg), 0);
    if (ret == -1) {
        std::cout << "errno = " << errno << "\n";
        throw std::runtime_error("send failed");
    }

    ret = send(sock, "\n", strlen("\n"), 0);
    if (ret == -1) {
        std::cout << "errno = " << errno << "\n";
        throw std::runtime_error("send failed");
    }
    
    std::cout << "write msg : " << msg << std::endl;
}

void Socket_client::send_msg(const std::string& msg) {
    const char* msg_buf = msg.c_str();
    int ret = send(sock, msg_buf, strlen(msg_buf), 0);
    if (ret == -1) {
        std::cout << "errno = " << errno << "\n";
        throw std::runtime_error("send failed");
    }

    ret = send(sock, "\n", strlen("\n"), 0);
    if (ret == -1) {
        std::cout << "errno = " << errno << "\n";
        throw std::runtime_error("send failed");
    }
    
    std::cout << "write msg : " << msg << std::endl;
}

void Socket_client::read_msg() {
    bool newline_visited = false;
    memset(buf, 0, sizeof(buf));
    size_t buf_end = 0;
    
    while (!newline_visited || buf_end == 0) {
        newline_visited = false;
        // recv from socket if all characters in temp_buf are read already
        if (temp_buf_unread_begin == temp_buf_unread_end) {
            memset(temp_buf, 0, sizeof(temp_buf));
            int ret = recv(sock, temp_buf, sizeof(temp_buf), 0);
            // std::cout << "recv msg : " << temp_buf << "\n";
            if (ret == -1) {
                std::cout << "errno = " << errno << "\n";
                throw std::runtime_error("recv failed");
            }

            temp_buf_unread_begin = 0;
            temp_buf_unread_end = ret;
        }

        // copy from temp_buf characters before newline
        for (size_t i = temp_buf_unread_begin; i < temp_buf_unread_end; i++) {
            char c = temp_buf[i];
            if (c == '\n') {
                // copy before \n
                int copy_len = i - temp_buf_unread_begin;

                if (buf_end + copy_len >= bufsize) {
                    std::cout << "1 " << buf_end << " " << copy_len << " " << bufsize << std::endl;
                    throw std::runtime_error("error msg greater than bufsize");
                }
		
                memcpy(buf + buf_end, temp_buf + temp_buf_unread_begin, copy_len);
                buf_end += copy_len;
                buf[buf_end] = '\0';
                temp_buf_unread_begin = i + 1;
                newline_visited = true;
                break;
            }
        }
	
        if (!newline_visited) {
            int copy_len = temp_buf_unread_end - temp_buf_unread_begin;
            // leave space for '\0'
            if (buf_end + copy_len >= bufsize) {
                std::cout << "2 " << buf_end << " " << copy_len << " " << bufsize << std::endl;

                throw std::runtime_error("error msg greater than bufsize");
            }
		
            memcpy(buf + buf_end, temp_buf + temp_buf_unread_begin, copy_len);
            buf_end += copy_len;
            temp_buf_unread_begin = temp_buf_unread_end;
        }
    }

    fmt::print("begin {} end {}\n", temp_buf_unread_begin, temp_buf_unread_end);
    std::cout << "read msg : " << buf << "\n";
}

bool Socket_client::recv_msg() {
    // set socket to non blocking mode before use
    // int flags = MSG_DONTWAIT;
    int ret = recv(sock, buf, sizeof(buf), 0);
    if (ret == -1) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return false;
        }

        throw std::runtime_error("recv failed");
    }
    
    // std::cout << "recv returned : " << ret << std::endl;
    // std::cout << buf << std::endl;
    return true;
}
