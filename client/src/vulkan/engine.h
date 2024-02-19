#pragma once

#include <iostream>
#include <thread>
#include <vector>
#include <optional>
#include <set>
#include <string>
#include <array>

#define FMT_HEADER_ONLY
#include <fmt/core.h>
#include <fmt/ranges.h>
#include <fmt/format.h>

#include <vulkan/vulkan_core.h>
#include <vulkan/vulkan.h>
#include "../glfw/glfw_window.h"
#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>
#define GLM_FORCE_RADIANS
#define GLM_FORCE_DEPTH_ZERO_TO_ONE
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#define GLM_ENABLE_EXPERIMENTAL
#include <glm/gtx/hash.hpp>

#include "../font/font.h"

using namespace glfw_window;
using namespace font;

namespace engine {
    // #define NDEBUG
#ifdef NDEBUG
    const bool enableValidationLayers = false;
#else
    const bool enableValidationLayers = true;
#endif

    const std::vector<const char*> validationLayers = {
	"VK_LAYER_KHRONOS_validation"
    };

    const std::vector<const char*> deviceExtensions = {
	VK_KHR_SWAPCHAIN_EXTENSION_NAME
    };

    const int MAX_FRAMES_IN_FLIGHT = 2;

    struct QueueFamilyIndices {
	std::optional<uint32_t> graphicsFamily;
	std::optional<uint32_t> presentFamily;

	bool isComplete() {
            return graphicsFamily.has_value() && presentFamily.has_value();
	}
    };

    struct SwapChainSupportDetails {
	VkSurfaceCapabilitiesKHR capabilities;
	std::vector<VkSurfaceFormatKHR> formats;
	std::vector<VkPresentModeKHR> presentModes;
    };

    struct Vertex {
	glm::vec4 pos;
	glm::vec2 texCoord;

	static VkVertexInputBindingDescription getBindingDescription() {
            VkVertexInputBindingDescription bindingDescription{};
            bindingDescription.binding = 0;
            bindingDescription.stride = sizeof(Vertex);
            bindingDescription.inputRate = VK_VERTEX_INPUT_RATE_INSTANCE;

            return bindingDescription;
	}

	static std::array<VkVertexInputAttributeDescription, 2> getAttributeDescriptions() {
            std::array<VkVertexInputAttributeDescription, 2> attributeDescriptions{};

            attributeDescriptions[0].binding = 0;
            attributeDescriptions[0].location = 0;
            attributeDescriptions[0].format = VK_FORMAT_R32G32B32A32_SFLOAT;
            attributeDescriptions[0].offset = offsetof(Vertex, pos);

	    attributeDescriptions[1].binding = 0;
            attributeDescriptions[1].location = 1;
            attributeDescriptions[1].format = VK_FORMAT_R32G32_SFLOAT;
            attributeDescriptions[1].offset = offsetof(Vertex, texCoord);

            return attributeDescriptions;
	}

	bool operator==(const Vertex& other) const {
            return pos == other.pos && texCoord == other.texCoord;
	}
    };

    const std::vector<Vertex> vertices {
	{ { -1.0f, -1.0f, 0.0f, 1.0f }, { 0.0f, 1.0f } },
	{ { 1.0f, -1.0f, 0.0f, 1.0f }, { 1.0f, 1.0f } },
	{ { 1.0f, 1.0f, 0.0f, 1.0f }, { 1.0f, 0.0f } },
	{ { -1.0f, 1.0f, 0.0f, 1.0f }, { 0.0f, 0.0f } }
    };

    const std::vector<uint32_t> indices {
	0, 1, 3, 1, 2, 3
    };

    struct InstanceData {
	glm::mat4 modelMatrix;
	glm::vec3 color;
	int char_index;
    };

    class InstanceDataContainer {
    public:
	void insert(const InstanceData& data);
	void reset();
	void clear();
	InstanceData* get_pointer();
	int get_size();
	void update();
	
    private:
	int curr_size = 0;
	int buffer_size = 0;
	std::vector<InstanceData> data_buffer;	
    };

    struct UniformBufferObject {
	glm::mat4 view;
	glm::mat4 proj;
    };
    
    class Engine {
    public:
	Engine() = delete;
	Engine(int width, int height, const char* title, void (*func)(GLFWwindow* window, int key, int scancode, int action, int mods)) {
	    glfwInit();
            glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
            window = glfwCreateWindow(width, height, title, nullptr, nullptr);
            glfwSetWindowUserPointer(window, nullptr);
            // glfwSetFramebufferSizeCallback(window, framebufferResizeCallback);
	    glfwSetKeyCallback(window, func);
	}

	GLFWwindow* window;
	
	static VKAPI_ATTR VkBool32 VKAPI_CALL debugCallback(VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity, VkDebugUtilsMessageTypeFlagsEXT messageType, const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData, void* pUserData);

	void run() {
	    init_font();
	    init_vulkan();
	    main_loop();
	    clean_up();
	}
	
    private:
	FontInfo font_info;
	
	VkInstance instance;
	VkSurfaceKHR surface;
	VkDebugUtilsMessengerEXT debugMessenger;

	VkPhysicalDevice physicalDevice;
	VkDevice device;

	VkQueue graphicsQueue;
	VkQueue presentQueue;

	VkSwapchainKHR swapChain;
	std::vector<VkImage> swapChainImages;
	VkFormat swapChainImageFormat;
	VkExtent2D swapChainExtent;
	std::vector<VkImageView> swapChainImageViews;
	VkRenderPass renderPass;
	std::vector<VkFramebuffer> swapChainFramebuffers;

	VkCommandPool commandPool;
	std::vector<VkCommandBuffer> commandBuffers;

	InstanceDataContainer instanceDataContainer;

	VkBuffer vertexBuffer;
	VkDeviceMemory vertexBufferMemory;

	VkBuffer indexBuffer;
	VkDeviceMemory indexBufferMemory;

	std::vector<VkBuffer> uniformBuffers;
	std::vector<VkDeviceMemory> uniformBuffersMemory;
	std::vector<void*> uniformBuffersMapped;

	std::vector<VkBuffer> fontBuffers;
	std::vector<VkDeviceMemory> fontBuffersMemory;

	VkDescriptorSetLayout descriptorSetLayout;
	VkDescriptorPool descriptorPool;
	std::vector<VkDescriptorSet> descriptorSets;

	
	
	void init_font();
	
	void init_vulkan() {
	    createInstance();
	    createSurface();

	    setupDebugMessenger();

	    pickPhysicalDevice();
	    createLogicalDevice();

	    createImageViews();
	    createSwapChain();
	    createRenderPass();

	    createFramebuffers();
	    createCommandPool();
	    createCommandBuffers();

	    createVertexBuffer();
	    createIndexBuffer();
	    createUniformBuffers();
	    createFontBuffers();

	    createDescriptorSetLayout();
	    
	    // draw
	    updateInstanceDataContainer();
	}
	
	void main_loop() {
	    std::thread { [&] {
		
	    } }.detach();
	    
	    while (!glfwWindowShouldClose(window)) {
		glfwPollEvents();
	    }
	}
	
	void clean_up() {
	    for (int i = 0; i < 12; i++) {
		vkDestroyBuffer(device, fontBuffers[i], nullptr);
		vkFreeMemory(device, fontBuffersMemory[i], nullptr);
	    }

	    for (size_t i = 0; i < MAX_FRAMES_IN_FLIGHT; i++) {
		vkDestroyBuffer(device, uniformBuffers[i], nullptr);
		vkFreeMemory(device, uniformBuffersMemory[i], nullptr);
            }

	    vkDestroyBuffer(device, indexBuffer, nullptr);
            vkFreeMemory(device, indexBufferMemory, nullptr);

            vkDestroyBuffer(device, vertexBuffer, nullptr);
            vkFreeMemory(device, vertexBufferMemory, nullptr);
	    
	    vkDestroyCommandPool(device, commandPool, nullptr);
	    for (auto framebuffer : swapChainFramebuffers) {
		vkDestroyFramebuffer(device, framebuffer, nullptr);
	    }
	    vkDestroyRenderPass(device, renderPass, nullptr);
	    cleanupSwapChain();
	    vkDestroyDevice(device, nullptr);
	    vkDestroySurfaceKHR(instance, surface, nullptr);
	    if (enableValidationLayers) {
		DestroyDebugUtilsMessengerEXT(instance, debugMessenger, nullptr);
	    }
	    vkDestroyInstance(instance, nullptr);
	}

	void createInstance();
	void createSurface();

	void setupDebugMessenger();
	void DestroyDebugUtilsMessengerEXT(VkInstance instance, VkDebugUtilsMessengerEXT debugMessenger, const VkAllocationCallbacks* pAllocator);

	void pickPhysicalDevice();
	void createLogicalDevice();
	void createSwapChain();

	VkImageView createImageView(VkImage image, VkFormat format, VkImageAspectFlags aspectFlags, uint32_t mipLevels);
	void createImageViews();
	void cleanupSwapChain();
	
	VkFormat findSupportedFormat(const std::vector<VkFormat>& candidates, VkImageTiling tiling, VkFormatFeatureFlags features);
	VkFormat findDepthFormat();
	void createRenderPass();
	
	void createFramebuffers();
	void createCommandPool();
	void createCommandBuffers();

	void updateInstanceDataContainer();

	uint32_t findMemoryType(uint32_t typeFilter, VkMemoryPropertyFlags properties);
	void createBuffer(VkDeviceSize size, VkBufferUsageFlags usage, VkMemoryPropertyFlags properties, VkBuffer& buffer, VkDeviceMemory& bufferMemory);
	VkCommandBuffer beginSingleTimeCommands();
	void endSingleTimeCommands(VkCommandBuffer commandBuffer);
	void copyBuffer(VkBuffer srcBuffer, VkBuffer dstBuffer, VkDeviceSize size);
	
	void createVertexBuffer();
	void createIndexBuffer();
	void createUniformBuffers();
	void createFontBuffers();

	void createDescriptorSetLayout();
	void createDescriptorPool();
	void createDescriptorSets();

	
	
	void recordCommandBuffer(VkCommandBuffer commandBuffer, uint32_t imageIndex);
	
    };
}
