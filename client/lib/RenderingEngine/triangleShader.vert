#version 450
#extension GL_EXT_debug_printf : enable

layout(location = 0) in vec4 inPos;
layout(location = 1) in vec4 inColor;

layout(binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 view;
    mat4 proj;
} ubo;

layout(location = 0) out vec4 fragColor;

void main() {
    gl_Position = ubo.proj * ubo.view * ubo.model * inPos;
    fragColor = inColor;
}
