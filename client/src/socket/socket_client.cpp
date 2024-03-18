#include "socket_client.h"
#include <cerrno>
#include <regex>
#include <cstring>
#include <iostream>

#include <errno.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>

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

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_family = AF_INET;
    int ret = getaddrinfo(server_ip.c_str(), server_port.c_str(), &hints, &res);
    if (ret != 0) {
	std::cout << "errno = " << errno << "\n";
        throw std::runtime_error("getaddrinfo failed");
    }

    bool connect_succeed = false;
    for (rp = res; rp != NULL; rp = rp->ai_next) {
	socket_fd = socket(AF_INET, SOCK_STREAM, 0);
	if (socket_fd == -1)
	    continue;

	// Set socket to non-blocking mode
	// int flags = fcntl(socket_fd, F_GETFL, 0);
	// if (flags == -1) {
        //     throw std::runtime_error("Error getting socket flags\n");
	// }
	// if (fcntl(socket_fd, F_SETFL, flags | O_NONBLOCK) == -1) {
        //     throw std::runtime_error("Error setting socket to non-blocking mode\n");
	// }
	
	if (connect(socket_fd, rp->ai_addr, rp->ai_addrlen) != -1) {
	    connect_succeed = true;
	    break; 
	}

	// Wait for connection to complete
	if (errno == EINPROGRESS) {
            fd_set writefds;
            FD_ZERO(&writefds);
            FD_SET(socket_fd, &writefds);
        
            if (select(socket_fd + 1, NULL, &writefds, NULL, NULL) < 0) {
		std::runtime_error("Error in select\n");
            }

            if (FD_ISSET(socket_fd, &writefds)) {
		int so_error;
		socklen_t len = sizeof so_error;
		getsockopt(socket_fd, SOL_SOCKET, SO_ERROR, &so_error, &len);
		if (so_error != 0) {
                    std::runtime_error("Error connecting to server\n");
		}

		connect_succeed = true;
		break; 
            }
	}

	close (socket_fd);
    }
    
    freeaddrinfo(res);

    if (!connect_succeed)
	throw std::runtime_error("connect failed");
}

Socket_client::Socket_client(const std::string& ip_port) {
    init(ip_port);
}

Socket_client::~Socket_client() {
    close(socket_fd);
}

void Socket_client::send_msg(const char *msg) {
    int ret = send(socket_fd, msg, strlen(msg), 0);
    if (ret == -1) {
        std::cout << "errno = " << errno << "\n";
        throw std::runtime_error("send failed");
    }

    ret = send(socket_fd, "\n", strlen("\n"), 0);
    if (ret == -1) {
        std::cout << "errno = " << errno << "\n";
        throw std::runtime_error("send failed");
    }
    
    std::cout << "write msg : " << msg << std::endl;
}

void Socket_client::send_msg(const std::string& msg) {
    const char* msg_buf = msg.c_str();
    int ret = send(socket_fd, msg_buf, strlen(msg_buf), 0);
    if (ret == -1) {
        std::cout << "errno = " << errno << "\n";
        throw std::runtime_error("send failed");
    }

    ret = send(socket_fd, "\n", strlen("\n"), 0);
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
	if (temp_buf_unread_begin == temp_buf_unread_end) {
	    memset(temp_buf, 0, sizeof(temp_buf));
	    int ret = recv(socket_fd, temp_buf, sizeof(temp_buf), 0);
	    if (ret == -1) {
		std::cout << "errno = " << errno << "\n";
		throw std::runtime_error("recv failed");
	    }

	    temp_buf_unread_begin = 0;
	    temp_buf_unread_end = ret;
	}

	for (size_t i = temp_buf_unread_begin; i < temp_buf_unread_end; i++) {
	    char c = temp_buf[i];
	    if (c == '\n') {
		// copy before \n
		int copy_len = i - temp_buf_unread_begin;
		if (copy_len == 0) {
		    temp_buf_unread_begin = i + 1;
		    continue;
		}
		// leave space for '\0'
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
    
    std::cout << "read msg : " << buf << std::endl;
}

bool Socket_client::recv_msg() {
    int flags = MSG_DONTWAIT;
    int ret = recv(socket_fd, buf, sizeof(buf), 0); 
    if (ret == -1) {
	if (errno == EAGAIN || errno == EWOULDBLOCK) {
	    return false;
	}

	throw std::runtime_error("recv failed");
    }
    
    std::cout << "recv returned : " << ret << std::endl;
    std::cout << buf << std::endl;
    return true;
}
