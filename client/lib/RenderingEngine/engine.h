#pragma once

#include <cstdint>
#include <vector>
#include <optional>
#include <mutex>

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
    
    struct TriangleVertex {
	glm::vec4 pos;
	glm::vec4 color;
    };
    
    struct GlyphBoxVertex {
	glm::vec4 pos;
	glm::vec2 texCoord;

	bool operator==(const GlyphBoxVertex& other) const {
            return pos == other.pos && texCoord == other.texCoord;
	}
    };
    
    struct CharacterObject {
	alignas(16) glm::mat4 model;
	alignas(16) glm::mat4 view;
	alignas(16) glm::mat4 proj;
	alignas(16) glm::vec4 color;
	alignas(16) int charId;
    };
    
    struct TransformMatrixObject {
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

	    createGlobalTransformMatrixUniformBuffers();

	    createTriangleVertexBuffer();
	    updateTriangleVertexBuffer();
	    
	    createTriangleIndexBuffer();
	    updateTriangleIndexBuffer();
	    
	    createGlyphBoxVertexBuffer();
	    createGlyphBoxIndexBuffer();

	    createTextStorageBuffer();
	    updateTextStorageBuffer();

	    // glyphs' bezier curves information for vector rendering
	    createFontInfoStorageBuffer();
	    
	    createTriangleDescriptorSetLayout();
	    createTriangleDescriptorPool();
	    allocateTriangleDescriptorSets();
	    updateTriangleDescriptorSets();

	    createTextDescriptorSetLayout();
	    createTextDescriptorPool();
	    allocateTextDescriptorSets();
	    updateTextDescriptorSets();
	    
	    createTriangleGraphicsPipeline();
	    createTextGraphicsPipeline();    
	    
	    createSyncObjects();
	}
	
	void run() {
	    main_loop();
	    clean_up();
	}
	
	Socket_client client;
	FontInfo fontInfo;
	GLFWwindow* window;

	std::mutex triangleVertexBufferMutex;

	std::vector<TriangleVertex> triangleVertices;
	int maxTriangleVertexCount = 100;
	int triangleVertexCount;
	
	std::vector<uint32_t> triangleIndices;
	int maxTriangleIndexCount = 100;
	int triangleIndexCount;
	
	std::mutex textStorageBufferMutex;

	std::vector<CharacterObject> characterObjects;
	int maxCharacterCount = 1;
	int characterCount = 0;
	
	bool framebufferResized = false;
	bool mouseLeftButtonPressed = false;

	bool triangleVertexBufferUpdateFlag = false;

	bool textStorageBufferUpdateFlag = false;
	bool textStorageBufferRecreateFlag = false;
	
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

	VkPipelineLayout trianglePipelineLayout;
	VkPipeline triangleGraphicsPipeline;

	VkPipelineLayout textPipelineLayout;
	VkPipeline textGraphicsPipeline;

	std::vector<VkFramebuffer> swapChainFramebuffers;
	
	VkCommandPool commandPool;
	std::vector<VkCommandBuffer> commandBuffers;
	
	VkBuffer triangleVertexBuffer;
	VkDeviceMemory triangleVertexBufferMemory;
	void* triangleVertexBufferMapped;
	
	VkBuffer triangleIndexBuffer;
	VkDeviceMemory triangleIndexBufferMemory;
	void* triangleIndexBufferMapped;

	const std::vector<GlyphBoxVertex> glyphBoxVertices {
	    { { -1.0f, -1.0f, 0.0f, 1.0f }, { 0.0f, 0.0f } },
	    { { 1.0f, -1.0f, 0.0f, 1.0f }, { 1.0f, 0.0f } },
	    { { 1.0f, 1.0f, 0.0f, 1.0f }, { 1.0f, 1.0f } },
	    { { -1.0f, 1.0f, 0.0f, 1.0f }, { 0.0f, 1.0f } }
	};

	VkBuffer glyphBoxVertexBuffer;
	VkDeviceMemory glyphBoxVertexBufferMemory;

	const std::vector<uint32_t> glyphBoxIndices {
	    0, 1, 3, 1, 2, 3
	};
	
	VkBuffer glyphBoxIndexBuffer;
	VkDeviceMemory glyphBoxIndexBufferMemory;

	// vector size will be MAX_FRAMES_IN_FLIGHT
	std::vector<VkBuffer> globalTransformMatrixUniformBuffers;
	std::vector<VkDeviceMemory> globalTransformMatrixUniformBuffersMemory;
	std::vector<void*> globalTransformMatrixUniformBuffersMapped;

	VkBuffer textStorageBuffer;
	VkDeviceMemory textStorageBufferMemory;
	void* textStorageBufferMapped;

	std::vector<VkBuffer> fontInfoStorageBuffers;
	std::vector<VkDeviceMemory> fontInfoStorageBuffersMemory;
	std::vector<VkDeviceSize> fontInfoStorageBuffersSize;
	
	VkDescriptorSetLayout triangleDescriptorSetLayout;
	VkDescriptorPool triangleDescriptorPool;
	std::vector<VkDescriptorSet> triangleDescriptorSets;
	
	VkDescriptorSetLayout textDescriptorSetLayout;
	VkDescriptorPool textDescriptorPool;
	std::vector<VkDescriptorSet> textDescriptorSets;
 
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

	    vkDestroyPipeline(device, triangleGraphicsPipeline, nullptr);
            vkDestroyPipelineLayout(device, trianglePipelineLayout, nullptr);

	    vkDestroyPipeline(device, textGraphicsPipeline, nullptr);
            vkDestroyPipelineLayout(device, textPipelineLayout, nullptr);
	    
	    vkDestroyRenderPass(device, renderPass, nullptr);
   
	    vkFreeMemory(device, textStorageBufferMemory, nullptr);
	    vkDestroyBuffer(device, textStorageBuffer, nullptr);
	    
	    for (size_t i = 0; i < MAX_FRAMES_IN_FLIGHT; i++) {
		vkDestroyBuffer(device, globalTransformMatrixUniformBuffers[i], nullptr);
		vkFreeMemory(device, globalTransformMatrixUniformBuffersMemory[i], nullptr);
            }
	    
	    vkDestroyDescriptorPool(device, triangleDescriptorPool, nullptr);
            vkDestroyDescriptorSetLayout(device, triangleDescriptorSetLayout, nullptr);

	    vkDestroyDescriptorPool(device, textDescriptorPool, nullptr);
            vkDestroyDescriptorSetLayout(device, textDescriptorSetLayout, nullptr);

	    vkDestroyBuffer(device, triangleIndexBuffer, nullptr);
            vkFreeMemory(device, triangleIndexBufferMemory, nullptr);

            vkDestroyBuffer(device, glyphBoxVertexBuffer, nullptr);
            vkFreeMemory(device, glyphBoxVertexBufferMemory, nullptr);
	    
	    vkDestroyBuffer(device, glyphBoxIndexBuffer, nullptr);
            vkFreeMemory(device, glyphBoxIndexBufferMemory, nullptr);

            vkDestroyBuffer(device, glyphBoxVertexBuffer, nullptr);
            vkFreeMemory(device, glyphBoxVertexBufferMemory, nullptr);

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
	void recreateSwapChain();

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
	void recordCommandBuffer(VkCommandBuffer commandBuffer, uint32_t imageIndex);

	uint32_t findMemoryType(uint32_t typeFilter, VkMemoryPropertyFlags properties);
	void createBuffer(VkDeviceSize size, VkBufferUsageFlags usage, VkMemoryPropertyFlags properties, VkBuffer& buffer, VkDeviceMemory& bufferMemory);
	VkCommandBuffer beginSingleTimeCommands();
	void endSingleTimeCommands(VkCommandBuffer commandBuffer);
	void copyBuffer(VkBuffer srcBuffer, VkBuffer dstBuffer, VkDeviceSize size);
	
	void createTriangleVertexBuffer();
	void updateTriangleVertexBuffer();
	
	void createTriangleIndexBuffer();
	void updateTriangleIndexBuffer();

	void createGlyphBoxVertexBuffer();
	void createGlyphBoxIndexBuffer();

	void createGlobalTransformMatrixUniformBuffers();
	void updateGlobalTransformMatrixUniformBuffer(uint32_t currentImage);

	void createTextStorageBuffer();
	void updateTextStorageBuffer();
	void recreateTextStorageBuffer();

	void createFontInfoStorageBuffer();
	
	void createTriangleDescriptorSetLayout();
	void createTriangleDescriptorPool();
	void allocateTriangleDescriptorSets();
	void updateTriangleDescriptorSets();

	void createTextDescriptorSetLayout();
	void createTextDescriptorPool();
	void allocateTextDescriptorSets();
	void updateTextDescriptorSets();

	VkShaderModule createShaderModule(const std::vector<char>& code);

	void createTriangleGraphicsPipeline();
	void createTextGraphicsPipeline();

	void createSyncObjects();

	void drawFrame();
    };
}
