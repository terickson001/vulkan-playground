package main

import "core:time"

import "vendor:glfw"

global_input: Input;

Input :: struct
{
	scroll: [2]f32,
	scroll_next: [2]f32,
	mouse_button: [8]Button_State,
}

Button_State_Kind :: enum u8
{
	Up,
	Pressed,
	Down,
	Released,
}

Button_State :: struct
{
	time: time.Time, // Time when pressed
	pos: [2]f32,     // position when pressed
	state: Button_State_Kind,
}

init_input_handlers :: proc(window: glfw.WindowHandle)
{
    glfw.SetScrollCallback(window, scroll_handler);
}

input_end_frame :: proc(using input: ^Input)
{
    scroll = scroll_next;
    scroll_next = {};
}

scroll_handler :: proc "c" (window: glfw.WindowHandle, xoffset: f64, yoffset: f64)
{
    using global_input;
    scroll_next.y += f32(yoffset);
}
