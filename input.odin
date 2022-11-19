package main

import "core:time"

import "vendor:glfw"

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