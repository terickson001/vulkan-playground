#version 450

layout(binding = 0) uniform UniformBufferObject
{
	mat4 model;
	uvec2 res;
} ubo;

layout(location = 0) in vec3 in_position;
layout(location = 1) in vec4 in_color;
layout(location = 2) in vec2 in_uv;

layout(location = 0) out vec4 frag_color;
layout(location = 1) out vec2 frag_uv;

void main() 
{
	vec3 position = (ubo.model *  vec4(in_position, 1.0)).xyz;
	gl_Position = vec4((position*2) / vec3(ubo.res.x, ubo.res.y, 1) - vec3(1, 1, 0), 1);
	frag_color = in_color;
	frag_uv = in_uv;
}