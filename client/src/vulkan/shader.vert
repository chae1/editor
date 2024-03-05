#version 450
#extension GL_EXT_debug_printf : enable

layout(location = 0) in vec4 inPos;
layout(location = 1) in vec2 inTexCoord;

layout(binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 view;
    mat4 proj;
} ubo;

struct StorageBufferObject {
    mat4 model;
    mat4 view;
    mat4 proj;
    vec4 color;
    int charId;
};

layout(binding = 1) buffer StorageBuffer { StorageBufferObject ssbos[]; };
// layout(binding = 14) buffer offsetBuffer { int Offsets[]; };

layout(location = 0) out vec4 fragColor;
layout(location = 1) out vec2 fragTexCoord;
layout(location = 2) out int fragCharId;

void main() {
    StorageBufferObject ssbo = ssbos[gl_InstanceIndex];
    gl_Position =  ubo.proj * ubo.view * ubo.model * inPos;
    // gl_Position =  inPos;

    fragColor = ssbo.color;
    fragTexCoord = inTexCoord;
    fragCharId = ssbo.charId;
    
    // debugPrintfEXT("offset %d\n", Offsets[4]);

    // debugPrintfEXT("\nvertex shader\n");
    // debugPrintfEXT("gl_VertexIndex %d\n", gl_VertexIndex);
    debugPrintfEXT("inPos %1.2v4f\n", inPos);

    // debugPrintfEXT("ubo.model0 %1.2v4f\n", ubo.model[0]);
    // debugPrintfEXT("ubo.model1 %1.2v4f\n", ubo.model[1]);
    // debugPrintfEXT("ubo.model2 %1.2v4f\n", ubo.model[2]);
    // debugPrintfEXT("ubo.model3 %1.2v4f\n", ubo.model[3]);

    // debugPrintfEXT("ubo.view0 %1.2v4f\n", ubo.view[0]);
    // debugPrintfEXT("ubo.view1 %1.2v4f\n", ubo.view[1]);
    // debugPrintfEXT("ubo.view2 %1.2v4f\n", ubo.view[2]);
    // debugPrintfEXT("ubo.view3 %1.2v4f\n", ubo.view[3]);

    // debugPrintfEXT("ubo.proj0 %1.2v4f\n", ubo.proj[0]);
    // debugPrintfEXT("ubo.proj1 %1.2v4f\n", ubo.proj[1]);
    // debugPrintfEXT("ubo.proj2 %1.2v4f\n", ubo.proj[2]);
    // debugPrintfEXT("ubo.proj3 %1.2v4f\n", ubo.proj[3]);    
    
    // debugPrintfEXT("vertex %d gl_Position %1.2v4f\n", gl_VertexIndex, gl_Position);
    // debugPrintfEXT("color %1.2v4f\n", ssbo.color);
    // debugPrintfEXT("texCoord %1.2v2f\n", inTexCoord);    
}
