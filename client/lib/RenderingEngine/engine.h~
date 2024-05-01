#pragma once

#include <iostream>
#include <thread>
#include <vector>
#include <optional>
#include <set>
#include <string>
#include <array>
#include <algorithm>

#include <fmt/core.h>
#include <fmt/ranges.h>
#include <fmt/format.h>

#include <vulkan/vulkan_core.h>
#include <vulkan/vulkan.h>

#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>

#define GLM_FORCE_RADIANS
#define GLM_FORCE_DEPTH_ZERO_TO_ONE
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#define GLM_ENABLE_EXPERIMENTAL
#include <glm/gtx/hash.hpp>
#include <glm/gtx/string_cast.hpp>

#include "fontInfo.h"
#include "socket_client.h"

using namespace font;
using namespace socket_client;

namespace vk_engine {

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

	bool operator==(const Vertex& other) const {
            return pos == other.pos && texCoord == other.texCoord;
	}
    };
    
    struct StorageBufferObject {
	alignas(16) glm::mat4 model;
	alignas(16) glm::mat4 view;
	alignas(16) glm::mat4 proj;
	alignas(16) glm::vec4 color;
	alignas(16) int charId;
    };
    
    struct UniformBufferObject {
	glm::mat4 model;
	glm::mat4 view;
	glm::mat4 proj;
    };
    
    class Engine {
    public:
	Engine() = delete;
	Engine(int width, int height, const char* title, void (*key_callback)(GLFWwindow* window, int key, int scancode, int action, int mods), void (*framebufferResizeCallback)(GLFWwindow* window, int width, int height), void (*mouseButtonCallback)(GLFWwindow* window, int button, int action, int mode), void (*recreateSwapChainCallback)(), void (*socket_listener)()) {
	    glfwInit();
            glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
            window = glfwCreateWindow(width, height, title, nullptr, nullptr);
            glfwSetWindowUserPointer(window, this);
            glfwSetFramebufferSizeCallback(window, framebufferResizeCallback);
	    glfwSetMouseButtonCallback(window, mouseButtonCallback);
	    glfwSetKeyCallback(window, key_callback);

	    this->socket_listener = socket_listener;
	    this->recreateSwapChainCallback = recreateSwapChainCallback;
	}
	
	~Engine() {
	    glfwDestroyWindow(window);
            glfwTerminate();
	}

	void init_font();
	
	void init_vulkan() {
	    createInstance();
	    createSurface();

	    setupDebugMessenger();

	    pickPhysicalDevice();
	    createLogicalDevice();

	    createSwapChain();
	    createImageViews();
	    
	    createRenderPass();

	    createColorResources();
	    createDepthResources();	    
	    createFramebuffers();
	    
	    createCommandPool();
	    createCommandBuffers();

	    createVertexBuffer();
	    createIndexBuffer();

	    createStorageBuffer();
	    initStorageBuffer();
	    
	    createUniformBuffers();
	    createGlyphBuffers();
	    
	    createDescriptorSetLayout();
	    createDescriptorPool();
	    allocateDescriptorSets();
	    updateDescriptorSets();
	    
	    createGraphicsPipeline();
	    createSyncObjects();
	}
	
	void run() {
	    main_loop();
	    clean_up();
	}

	void recreateStorageBuffer();
	void updateStorageBuffer();
	
	Socket_client client;
	FontInfo fontInfo;

	GLFWwindow* window;
	
	int maxSsboCount = 1;
	int ssboCount = 1;
	std::vector<StorageBufferObject> objs;
	
	bool framebufferResized = false;
	bool mouseLeftButtonPressed = false;
	bool storageBufferRecreateFlag = false;
	bool storageBufferUpdateFlag = false;
	
    private:
	void (*socket_listener)();
	void (*recreateSwapChainCallback)();
	

	VkInstance instance;
	VkSurfaceKHR surface;
	VkDebugUtilsMessengerEXT debugMessenger;

	VkSampleCountFlagBits msaaSamples = VK_SAMPLE_COUNT_1_BIT;
	VkPhysicalDevice physicalDevice;
	VkDevice device;

	VkQueue graphicsQueue;
	VkQueue presentQueue;

	uint32_t mipLevels = 1;
	
	VkSwapchainKHR swapChain;
	std::vector<VkImage> swapChainImages;
	VkFormat swapChainImageFormat;
	VkExtent2D swapChainExtent;
	std::vector<VkImageView> swapChainImageViews;
	VkRenderPass renderPass;

	VkImage colorImage;
	VkDeviceMemory colorImageMemory;
	VkImageView colorImageView;
	
	VkImage depthImage;
	VkDeviceMemory depthImageMemory;
	VkImageView depthImageView;

	VkPipelineLayout pipelineLayout;
	VkPipeline graphicsPipeline;	

	std::vector<VkFramebuffer> swapChainFramebuffers;
	
	VkCommandPool commandPool;
	std::vector<VkCommandBuffer> commandBuffers;
	
	const std::vector<Vertex> vertices {
	    { { -1.0f, -1.0f, 0.0f, 1.0f }, { 0.0f, 0.0f } },
	    { { 1.0f, -1.0f, 0.0f, 1.0f }, { 1.0f, 0.0f } },
	    { { 1.0f, 1.0f, 0.0f, 1.0f }, { 1.0f, 1.0f } },
	    { { -1.0f, 1.0f, 0.0f, 1.0f }, { 0.0f, 1.0f } }
	};

	VkBuffer vertexBuffer;
	VkDeviceMemory vertexBufferMemory;
	
	VkBuffer storageBuffer;
	VkDeviceMemory storageBufferMemory;
	void* storageBufferMapped;

	const std::vector<uint32_t> indices {
	    0, 1, 3, 1, 2, 3
	};
	
	VkBuffer indexBuffer;
	VkDeviceMemory indexBufferMemory;

	std::vector<VkBuffer> uniformBuffers;
	std::vector<VkDeviceMemory> uniformBuffersMemory;
	std::vector<void*> uniformBuffersMapped;
	
	std::vector<VkBuffer> glyphBuffers;
	std::vector<VkDeviceMemory> glyphBuffersMemory;
	std::vector<uint32_t> glyphBuffersElementSize;
	std::vector<size_t> glyphBuffersLength;
	std::vector<VkDeviceSize> glyphBuffersSize;
	
	VkDescriptorSetLayout descriptorSetLayout;
	VkDescriptorPool descriptorPool;
	std::vector<VkDescriptorSet> descriptorSets;
	
	std::vector<VkSemaphore> imageAvailableSemaphores;
	std::vector<VkSemaphore> renderFinishedSemaphores;
	std::vector<VkFence> inFlightFences;
	uint32_t currentFrame = 0;
		
	void main_loop() {
	    // std::thread { socket_listener }.detach();
	    
	    while (!glfwWindowShouldClose(window)) {
		glfwPollEvents();
		drawFrame();
	    }
	    
	    vkDeviceWaitIdle(device);
	}
	
	void clean_up() {
	    cleanupSwapChain();

	    vkDestroyPipeline(device, graphicsPipeline, nullptr);
            vkDestroyPipelineLayout(device, pipelineLayout, nullptr);
	    vkDestroyRenderPass(device, renderPass, nullptr);

	    vkDestroyBuffer(device, storageBuffer, nullptr);
	    vkFreeMemory(device, storageBufferMemory, nullptr);
	    
	    for (size_t i = 0; i < MAX_FRAMES_IN_FLIGHT; i++) {
		vkDestroyBuffer(device, uniformBuffers[i], nullptr);
		vkFreeMemory(device, uniformBuffersMemory[i], nullptr);
            }
	    
	    vkDestroyDescriptorPool(device, descriptorPool, nullptr);
            vkDestroyDescriptorSetLayout(device, descriptorSetLayout, nullptr);

	    vkDestroyBuffer(device, indexBuffer, nullptr);
            vkFreeMemory(device, indexBufferMemory, nullptr);

            vkDestroyBuffer(device, vertexBuffer, nullptr);
            vkFreeMemory(device, vertexBufferMemory, nullptr);

	    for (size_t i = 0; i < MAX_FRAMES_IN_FLIGHT; i++) {
		vkDestroySemaphore(device, renderFinishedSemaphores[i], nullptr);
		vkDestroySemaphore(device, imageAvailableSemaphores[i], nullptr);
		vkDestroyFence(device, inFlightFences[i], nullptr);
            }
	    
	    vkDestroyCommandPool(device, commandPool, nullptr);

	    vkDestroyDevice(device, nullptr);
	    vkDestroySurfaceKHR(instance, surface, nullptr);
	    if (enableValidationLayers) {
		DestroyDebugUtilsMessengerEXT(instance, debugMessenger, nullptr);
	    }
	    vkDestroyInstance(instance, nullptr);

	    fmt::print("clean up vulkan\n");
	}

	void createInstance();
	void createSurface();

	static VKAPI_ATTR VkBool32 VKAPI_CALL debugCallback(VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity, VkDebugUtilsMessageTypeFlagsEXT messageType, const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData, void* pUserData);
	void populateDebugMessengerCreateInfo(VkDebugUtilsMessengerCreateInfoEXT& createInfo);
	void setupDebugMessenger();
	void DestroyDebugUtilsMessengerEXT(VkInstance instance, VkDebugUtilsMessengerEXT debugMessenger, const VkAllocationCallbacks* pAllocator);

	VkSampleCountFlagBits getMaxUsableSampleCount();
	void pickPhysicalDevice();
	void createLogicalDevice();
	void createSwapChain();

	VkImageView createImageView(VkImage image, VkFormat format, VkImageAspectFlags aspectFlags);
	void createImageViews();

	void cleanupSwapChain();
	
	VkFormat findSupportedFormat(const std::vector<VkFormat>& candidates, VkImageTiling tiling, VkFormatFeatureFlags features);
	VkFormat findDepthFormat();

	void createRenderPass();

	void createImage(uint32_t width, uint32_t height, VkFormat format, VkImageTiling tiling, VkImageUsageFlags usage, VkMemoryPropertyFlags properties, VkImage& image, VkDeviceMemory& imageMemory);
	void createColorResources();
	void createDepthResources();
	void createFramebuffers();

	void createCommandPool();
	void createCommandBuffers();

	uint32_t findMemoryType(uint32_t typeFilter, VkMemoryPropertyFlags properties);
	void createBuffer(VkDeviceSize size, VkBufferUsageFlags usage, VkMemoryPropertyFlags properties, VkBuffer& buffer, VkDeviceMemory& bufferMemory);
	VkCommandBuffer beginSingleTimeCommands();
	void endSingleTimeCommands(VkCommandBuffer commandBuffer);
	void copyBuffer(VkBuffer srcBuffer, VkBuffer dstBuffer, VkDeviceSize size);
	
	void createVertexBuffer();
	void createIndexBuffer();
	
	void createUniformBuffers();

	void createStorageBuffer();
	void initStorageBuffer();
	
	void createGlyphBuffers();
	
	void createDescriptorSetLayout();
	void createDescriptorPool();

	void allocateDescriptorSets();
	void updateDescriptorSets();

	VkShaderModule createShaderModule(const std::vector<char>& code);
	void createGraphicsPipeline();
	void createSyncObjects();

	void recreateSwapChain();

	void updateUniformBuffer(uint32_t currentImage);

	void recordCommandBuffer(VkCommandBuffer commandBuffer, uint32_t imageIndex);
	void drawFrame();
    };
}
