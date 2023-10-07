package main

import "core:fmt"
import "core:os"
import "core:sys/windows"
import "core:strings"
import "core:mem"
import "core:math/linalg/glsl"
import "core:time"
import "core:image"
import "core:image/png"
import "core:math"

import "shared:shaderc"

import "vendor:glfw"
import vk "vendor:vulkan"

MAX_FRAMES_IN_FLIGHT :: 2;

Context :: struct
{
    instance: vk.Instance,
    device:   vk.Device,
	physical_device: vk.PhysicalDevice,
	swap_chain: Swapchain,
	pipeline: Pipeline,
    queue_indices:   [QueueFamily]int,
    queues:   [QueueFamily]vk.Queue,
    surface:  vk.SurfaceKHR,
	window:   Window,
	command_pool: vk.CommandPool,
	command_buffers: [MAX_FRAMES_IN_FLIGHT]vk.CommandBuffer,
	descriptor_set_layout: vk.DescriptorSetLayout,
	descriptor_pool: vk.DescriptorPool,
	descriptor_sets: [MAX_FRAMES_IN_FLIGHT]vk.DescriptorSet,

	uniform_buffers: [MAX_FRAMES_IN_FLIGHT]VKBuffer,
	samplers: [1]vk.Sampler,

	image_available: [MAX_FRAMES_IN_FLIGHT]vk.Semaphore,
	render_finished: [MAX_FRAMES_IN_FLIGHT]vk.Semaphore,
	in_flight: [MAX_FRAMES_IN_FLIGHT]vk.Fence,

	curr_frame: u32,
	framebuffer_resized: bool,

	using state: State,
}

Window :: struct
{
	handle: glfw.WindowHandle,
	res: [2]uint,

}

State :: struct
{
	timer: Timer,
	input: Input,
	renderers: [MAX_FRAMES_IN_FLIGHT]Renderer,
}

Timer :: struct
{
	start  : time.Time,
	current: time.Time,
	total  : time.Duration,
	frame  : time.Duration,
}

VKBuffer :: struct
{
	buffer: vk.Buffer,
	memory: vk.DeviceMemory,
	length: int,
	size:   vk.DeviceSize,
}

Image :: struct
{
	handle: vk.Image,
	memory: vk.DeviceMemory,
	format: vk.Format,
	view  : vk.ImageView,
	dim   : [2]u32,
}

Pipeline :: struct
{
	handle: vk.Pipeline,
	render_pass: vk.RenderPass,
	layout: vk.PipelineLayout,
}

QueueFamily :: enum
{
	Graphics,
	Present,
}

Swapchain :: struct
{
	handle: vk.SwapchainKHR,
	images: []vk.Image,
	image_views: []vk.ImageView,
	depth_image: Image,
	format: vk.SurfaceFormatKHR,
	extent: vk.Extent2D,
	present_mode: vk.PresentModeKHR,
	image_count: u32,
	support: SwapChainDetails,
	framebuffers: []vk.Framebuffer,
}

SwapChainDetails :: struct
{
	capabilities: vk.SurfaceCapabilitiesKHR,
	formats: []vk.SurfaceFormatKHR,
	present_modes: []vk.PresentModeKHR,
}

Vertex :: struct
{
	pos: [3]f32,
	color: [4]f32,
	uv: [2]f32,
}

Rect :: struct
{
	tl: [2]f32,
	br: [2]f32,
}

Uniform_Buffer_Object :: struct
{
	model: glsl.mat4,
	res: [2]u32,
}

DEVICE_EXTENSIONS := [?]cstring{
	"VK_KHR_swapchain",
};

VALIDATION_LAYERS := [?]cstring{"VK_LAYER_KHRONOS_validation"};

main :: proc()
{
	using ctx: Context;
	init_window(&ctx);
	for q in &queue_indices do q = -1;

    init_input_handlers(window.handle);
	init_vulkan(&ctx);
	font := build_font(&ctx, "res/Inconsolata-Regular.ttf", 24);

	upload_texture(&ctx, font.image, 0);

	for r in &renderers
	{
		r.default_font = &font;
		r.window = window;
	}

	ctx.timer.start = time.now();

    test_pane: Pane;
	test_buffer: Buffer;
	test_pane.buffer = &test_buffer;

	ok: bool;
	test_buffer.text, ok = os.read_entire_file("main.odin");

	for !glfw.WindowShouldClose(window.handle)
	{
		vk.WaitForFences(device, 1, &in_flight[curr_frame], true, max(u64));
		last := ctx.timer.current;
		ctx.timer.current = time.now();
		ctx.timer.total = time.diff(ctx.timer.current, ctx.timer.start);
		ctx.timer.frame = time.diff(ctx.timer.current, last);

		test_pane.scroll += global_input.scroll * 20;
		glfw.PollEvents();
		renderer_start_frame(&ctx, &renderers[curr_frame], &font, window);
		pane_draw(&ctx, &test_pane);
		renderer_end_frame(&ctx, &renderers[curr_frame]);
		draw_frame(&ctx);
        input_end_frame(&global_input);
	}

	vk.DeviceWaitIdle(device);

	destroy_texture(&ctx, font.image);

	for r in &renderers do renderer_destroy(&ctx, &r);
	deinit_vulkan(&ctx);
	glfw.DestroyWindow(window.handle);
	glfw.Terminate();
}

VERTEX_BINDING := vk.VertexInputBindingDescription{
	binding = 0,
	stride = size_of(Vertex),
	inputRate = .VERTEX,
};

VERTEX_ATTRIBUTES := [?]vk.VertexInputAttributeDescription{
	{
		binding = 0,
		location = 0,
		format = .R32G32B32_SFLOAT,
		offset = cast(u32)offset_of(Vertex, pos),
	},
	{
		binding = 0,
		location = 1,
		format = .R32G32B32A32_SFLOAT,
		offset = cast(u32)offset_of(Vertex, color),
	},
	{
		binding = 0,
		location = 2,
		format = .R32G32_SFLOAT,
		offset = cast(u32)offset_of(Vertex, uv),
	}
};

init_window :: proc(using ctx: ^Context)
{
    glfw.Init();

    glfw.WindowHint(glfw.CLIENT_API, glfw.NO_API);
    glfw.WindowHint(glfw.RESIZABLE, 1);

	window.res = {800, 600};
	window.handle = glfw.CreateWindow(cast(i32)window.res.x, cast(i32)window.res.y, "Vulkan", nil, nil);
	glfw.SetWindowUserPointer(window.handle, ctx);
	glfw.SetFramebufferSizeCallback(window.handle, framebuffer_size_callback);
}

framebuffer_size_callback :: proc "c" (window: glfw.WindowHandle, width, height: i32)
{
	using ctx := cast(^Context)glfw.GetWindowUserPointer(window);
	framebuffer_resized = true;
}

init_vulkan :: proc(using ctx: ^Context)
{
	context.user_ptr = &instance;
	get_proc_address :: proc(p: rawptr, name: cstring)
	{
		(cast(^rawptr)p)^ = glfw.GetInstanceProcAddress((^vk.Instance)(context.user_ptr)^, name);
	}

	vk.load_proc_addresses(get_proc_address);
	create_instance(ctx);
	vk.load_proc_addresses(get_proc_address);

	extensions := get_extensions();
	for ext in &extensions do fmt.println(cstring(&ext.extensionName[0]));

	create_surface(ctx);
	get_suitable_device(ctx);
	find_queue_families(ctx);
	create_device(ctx);

	for q, f in &queues
	{
		vk.GetDeviceQueue(device, u32(queue_indices[f]), 0, &q);
	}

	create_swap_chain(ctx);
	create_image_views(ctx);
	create_uniform_buffers(ctx);
	create_descriptor_set_layout(ctx);
	create_descriptor_pool(ctx);
	create_descriptor_set(ctx);
	create_command_pool(ctx);
	create_depth_resources(ctx);
	create_graphics_pipeline(ctx, "shader.vert", "shader.frag");
	create_framebuffers(ctx);
	create_texture_samplers(ctx);
	create_command_buffers(ctx);
	create_sync_objects(ctx);

	return;
}

deinit_vulkan :: proc(using ctx: ^Context)
{
	cleanup_swap_chain(ctx);

	for sampler in samplers
	{
		vk.DestroySampler(device, sampler, nil);
	}

	for buffer in uniform_buffers
	{
		destroy_vkbuffer(ctx, buffer);
	}

	vk.DestroyDescriptorPool(device, descriptor_pool, nil);
	vk.DestroyDescriptorSetLayout(device, descriptor_set_layout, nil);

	vk.DestroyPipeline(device, pipeline.handle, nil);
	vk.DestroyPipelineLayout(device, pipeline.layout, nil);
	vk.DestroyRenderPass(device, pipeline.render_pass, nil);

	for i in 0..<MAX_FRAMES_IN_FLIGHT
	{
		vk.DestroySemaphore(device, image_available[i], nil);
		vk.DestroySemaphore(device, render_finished[i], nil);
		vk.DestroyFence(device, in_flight[i], nil);

	}
	vk.DestroyCommandPool(device, command_pool, nil);

	vk.DestroyDevice(device, nil);
	vk.DestroySurfaceKHR(instance, surface, nil);
	vk.DestroyInstance(instance, nil);
}

draw_frame :: proc(using ctx: ^Context)
{
	image_index: u32;

	res := vk.AcquireNextImageKHR(device, swap_chain.handle, max(u64), image_available[curr_frame], {}, &image_index);
	if res == .ERROR_OUT_OF_DATE_KHR || res == .SUBOPTIMAL_KHR || framebuffer_resized
	{
		fmt.print("RECREATING SWAPCHAIN... ");
		framebuffer_resized = false;
		recreate_swap_chain(ctx);
		for r in &renderers do r.window = window;
		fmt.println("DONE");
		return;
	}
	else if res != .SUCCESS
	{
		fmt.eprintf("Error: Failed to acquire swap chain image!\n");
		os.exit(1);
	}

	vk.ResetFences(device, 1, &in_flight[curr_frame]);
	vk.ResetCommandBuffer(command_buffers[curr_frame], {});
	assert(renderers[curr_frame].curr_job.vertex_buffer != {});
	renderer_record(ctx, &renderers[curr_frame], command_buffers[curr_frame], image_index);

	update_uniform_buffer(ctx);

	submit_info: vk.SubmitInfo;
	submit_info.sType = .SUBMIT_INFO;

	wait_semaphores := [?]vk.Semaphore{image_available[curr_frame]};
	wait_stages := [?]vk.PipelineStageFlags{{.COLOR_ATTACHMENT_OUTPUT}};
	submit_info.waitSemaphoreCount = 1;
	submit_info.pWaitSemaphores = &wait_semaphores[0];
	submit_info.pWaitDstStageMask = &wait_stages[0];
	submit_info.commandBufferCount = 1;
	submit_info.pCommandBuffers = &command_buffers[curr_frame];

	signal_semaphores := [?]vk.Semaphore{render_finished[curr_frame]};
	submit_info.signalSemaphoreCount = 1;
	submit_info.pSignalSemaphores = &signal_semaphores[0];

	if res := vk.QueueSubmit(queues[.Graphics], 1, &submit_info, in_flight[curr_frame]); res != .SUCCESS
	{
		fmt.eprintf("Error: Failed to submit draw command buffer!\n");
		os.exit(1);
	}

	present_info: vk.PresentInfoKHR;
	present_info.sType = .PRESENT_INFO_KHR;
	present_info.waitSemaphoreCount = 1;
	present_info.pWaitSemaphores = &signal_semaphores[0];

	swap_chains := [?]vk.SwapchainKHR{swap_chain.handle};
	present_info.swapchainCount = 1;
	present_info.pSwapchains = &swap_chains[0];
	present_info.pImageIndices = &image_index;
	present_info.pResults = nil;

	vk.QueuePresentKHR(queues[.Present], &present_info);
	curr_frame = (curr_frame + 1) % MAX_FRAMES_IN_FLIGHT;
}

compile_shader :: proc(name: string, kind: shaderc.shader_kind) -> []u8
{
	src_path := fmt.tprintf("./shaders/%s", name);
	cmp_path := fmt.tprintf("./shaders/compiled/%s.spv", name);
	src_time, src_err := os.last_write_time_by_name(src_path);
	if (src_err != os.ERROR_NONE)
	{
		fmt.eprintf("Failed to open shader %q\n", src_path);
		return nil;
	}


	cmp_time, cmp_err := os.last_write_time_by_name(cmp_path);
	if cmp_err == os.ERROR_NONE && cmp_time >= src_time
	{
		code, _ := os.read_entire_file(cmp_path);
		return code;
	}


	comp := shaderc.compiler_initialize();
	options := shaderc.compile_options_initialize();
	defer
	{
		shaderc.compiler_release(comp);
		shaderc.compile_options_release(options);
	}

	shaderc.compile_options_set_optimization_level(options, .performance);

	code, _ := os.read_entire_file(src_path);
	c_path := strings.clone_to_cstring(src_path, context.temp_allocator);
	res := shaderc.compile_into_spv(comp, cstring(raw_data(code)), len(code), kind, c_path, cstring("main"), options);
	defer shaderc.result_release(res);

	status := shaderc.result_get_compilation_status(res);
	if status != .success
	{
		fmt.printf("%s: Error: %s\n", name, shaderc.result_get_error_message(res));
		return nil;
	}

	length := shaderc.result_get_length(res);
	out := make([]u8, length);
	c_out := shaderc.result_get_bytes(res);
	mem.copy(raw_data(out), c_out, int(length));
	os.write_entire_file(cmp_path, out);

	return out;
}


/*
compile_shader :: proc(path: string) -> []u8
{
	code, ok := os.read_entire_file(path);
	if !ok
	{
		fmt.eprintf("Failed to open shader %q\n", path);
		return nil;
	}
	defer delete(code);

	input := glslang.input_t{
		language = glslang.SOURCE_GLSL,
		stage = glslang.STAGE_VERTEX,
		client = glslang.CLIENT_VULKAN,
		client_version = glslang.TARGET_VULKAN_1_0,
		target_language = glslang.TARGET_SPV,
		target_language_version = glslang.TARGET_SPV_1_5,
		code = cstring(&code[0]),
		default_version = 100,
		default_profile = glslang.CORE_PROFILE,
		force_default_version_and_profile = 0,
		forward_compatible = 0,
		messages = glslang.MSG_DEFAULT_BIT,
	};

	shader := glslang.shader_create(&input);
	defer glslang.shader_delete(shader);

	if res := glslang.shader_preprocess(shader, &input); res == 0
	{
		fmt.eprintf("GLSL preprocessing failed: %s\n", path);
		fmt.eprintf("%s\n", glslang.shader_get_info_log(shader));
		fmt.eprintf("%s\n", glslang.shader_get_info_debug_log(shader));
		fmt.eprintf("%s\n", code);
	}
	return nil;
}
*/

create_instance :: proc(using ctx: ^Context)
{
	app_info: vk.ApplicationInfo;
	app_info.sType = .APPLICATION_INFO;
	app_info.pApplicationName = "Hello Triangle";
	app_info.applicationVersion = vk.MAKE_VERSION(0, 0, 1);
	app_info.pEngineName = "No Engine";
	app_info.engineVersion = vk.MAKE_VERSION(1, 0, 0);
	app_info.apiVersion = vk.API_VERSION_1_0;

	create_info: vk.InstanceCreateInfo;
	create_info.sType = .INSTANCE_CREATE_INFO;
	create_info.pApplicationInfo = &app_info;
	glfw_ext := glfw.GetRequiredInstanceExtensions();
	create_info.ppEnabledExtensionNames = raw_data(glfw_ext);
	create_info.enabledExtensionCount = cast(u32)len(glfw_ext);

	when ODIN_DEBUG
	{
		layer_count: u32;
		vk.EnumerateInstanceLayerProperties(&layer_count, nil);
		layers := make([]vk.LayerProperties, layer_count);
		vk.EnumerateInstanceLayerProperties(&layer_count, raw_data(layers));

		outer: for name in VALIDATION_LAYERS
		{
			for layer in &layers
			{
				if name == cstring(&layer.layerName[0]) do continue outer;
			}
			fmt.eprintf("ERROR: validation layer %q not available\n", name);
			os.exit(1);
		}

		create_info.ppEnabledLayerNames = &VALIDATION_LAYERS[0];
		create_info.enabledLayerCount = len(VALIDATION_LAYERS);
		fmt.println("Validation Layers Loaded");
	}
	else
	{
		create_info.enabledLayerCount = 0;
	}

	if (vk.CreateInstance(&create_info, nil, &instance) != .SUCCESS)
	{
		fmt.eprintf("ERROR: Failed to create instance\n");
		return;
	}

	fmt.println("Instance Created");
}

get_extensions :: proc() -> []vk.ExtensionProperties
{
	n_ext: u32;
	vk.EnumerateInstanceExtensionProperties(nil, &n_ext, nil);
	extensions := make([]vk.ExtensionProperties, n_ext);
	vk.EnumerateInstanceExtensionProperties(nil, &n_ext, raw_data(extensions));

	return extensions;
}

create_surface :: proc(using ctx: ^Context)
{
	surface_create_info := vk.Win32SurfaceCreateInfoKHR{};
	surface_create_info.sType= .WIN32_SURFACE_CREATE_INFO_KHR;
	surface_create_info.hwnd = glfw.GetWin32Window(window.handle);
	surface_create_info.hinstance = cast(vk.HANDLE)windows.GetModuleHandleA(nil);

	if res := glfw.CreateWindowSurface(instance, window.handle, nil, &surface); res != .SUCCESS
	{
		fmt.eprintf("ERROR: Failed to create window surface\n");
		os.exit(1);
	}
}

check_device_extension_support :: proc(physical_device: vk.PhysicalDevice) -> bool
{
	ext_count: u32;
	vk.EnumerateDeviceExtensionProperties(physical_device, nil, &ext_count, nil);

	available_extensions := make([]vk.ExtensionProperties, ext_count);
	vk.EnumerateDeviceExtensionProperties(physical_device, nil, &ext_count, raw_data(available_extensions));

	for ext in DEVICE_EXTENSIONS
	{
		found: b32;
		for available in &available_extensions
		{
			if cstring(&available.extensionName[0]) == ext
			{
				found = true;
				break;
			}
		}
		if !found do return false;
	}
	return true;
}

get_suitable_device :: proc(using ctx: ^Context)
{
	device_count: u32;

	vk.EnumeratePhysicalDevices(instance, &device_count, nil);
	if device_count == 0
	{
		fmt.eprintf("ERROR: Failed to find GPUs with Vulkan support\n");
		os.exit(1);
	}
	devices := make([]vk.PhysicalDevice, device_count);
	vk.EnumeratePhysicalDevices(instance, &device_count, raw_data(devices));

	suitability :: proc(using ctx: ^Context, dev: vk.PhysicalDevice) -> int
	{
		props: vk.PhysicalDeviceProperties;
		features: vk.PhysicalDeviceFeatures;
		vk.GetPhysicalDeviceProperties(dev, &props);
		vk.GetPhysicalDeviceFeatures(dev, &features);

		score := 0;
		if props.deviceType == .DISCRETE_GPU do score += 1000;
		score += cast(int)props.limits.maxImageDimension2D;

		if !features.geometryShader do return 0;
		if !check_device_extension_support(dev) do return 0;
		if !features.samplerAnisotropy do return 0;

		query_swap_chain_details(ctx, dev);
		if len(swap_chain.support.formats) == 0 || len(swap_chain.support.present_modes) == 0 do return 0;

		return score;
	}

	hiscore := 0;
	for dev in devices
	{
		score := suitability(ctx, dev);
		if score > hiscore
		{
			physical_device = dev;
			hiscore = score;
		}
	}

	if (hiscore == 0)
	{
		fmt.eprintf("ERROR: Failed to find a suitable GPU\n");
		os.exit(1);
	}
}

find_queue_families :: proc(using ctx: ^Context)
{
	queue_count: u32;
	vk.GetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_count, nil);
	available_queues := make([]vk.QueueFamilyProperties, queue_count);
	vk.GetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_count, raw_data(available_queues));

	for v, i in available_queues
	{
		if .GRAPHICS in v.queueFlags && queue_indices[.Graphics] == -1 do queue_indices[.Graphics] = i;

		present_support: b32;
		vk.GetPhysicalDeviceSurfaceSupportKHR(physical_device, u32(i), surface, &present_support);
		if present_support && queue_indices[.Present] == -1 do queue_indices[.Present] = i;

		for q in queue_indices do if q == -1 do continue;
		break;
	}
}

create_device :: proc(using ctx: ^Context)
{
	unique_indices: map[int]b8;
	defer delete(unique_indices);
	for i in queue_indices do unique_indices[i] = true;

	queue_priority := f32(1.0);

	queue_create_infos: [dynamic]vk.DeviceQueueCreateInfo;
	defer delete(queue_create_infos);
	for k, _ in unique_indices
	{
		queue_create_info: vk.DeviceQueueCreateInfo;
		queue_create_info.sType = .DEVICE_QUEUE_CREATE_INFO;
		queue_create_info.queueFamilyIndex = u32(queue_indices[.Graphics]);
		queue_create_info.queueCount = 1;
		queue_create_info.pQueuePriorities = &queue_priority;
		append(&queue_create_infos, queue_create_info);
	}

	device_features: vk.PhysicalDeviceFeatures;
	device_features.samplerAnisotropy = true;

	device_create_info: vk.DeviceCreateInfo;
	device_create_info.sType = .DEVICE_CREATE_INFO;
	device_create_info.enabledExtensionCount = u32(len(DEVICE_EXTENSIONS));
	device_create_info.ppEnabledExtensionNames = &DEVICE_EXTENSIONS[0];
	device_create_info.pQueueCreateInfos = raw_data(queue_create_infos);
	device_create_info.queueCreateInfoCount = u32(len(queue_create_infos));
	device_create_info.pEnabledFeatures = &device_features;
	device_create_info.enabledLayerCount = 0;

	if vk.CreateDevice(physical_device, &device_create_info, nil, &device) != .SUCCESS
	{
		fmt.eprintf("ERROR: Failed to create logical device\n");
		os.exit(1);
	}
}

query_swap_chain_details :: proc(using ctx: ^Context, dev: vk.PhysicalDevice)
{
	vk.GetPhysicalDeviceSurfaceCapabilitiesKHR(dev, surface, &swap_chain.support.capabilities);

	format_count: u32;
	vk.GetPhysicalDeviceSurfaceFormatsKHR(dev, surface, &format_count, nil);
	if format_count > 0
	{
		swap_chain.support.formats = make([]vk.SurfaceFormatKHR, format_count);
		vk.GetPhysicalDeviceSurfaceFormatsKHR(dev, surface, &format_count, raw_data(swap_chain.support.formats));
	}

	present_mode_count: u32;
	vk.GetPhysicalDeviceSurfacePresentModesKHR(dev, surface, &present_mode_count, nil);
	if present_mode_count > 0
	{
		swap_chain.support.present_modes = make([]vk.PresentModeKHR, present_mode_count);
		vk.GetPhysicalDeviceSurfacePresentModesKHR(dev, surface, &present_mode_count, raw_data(swap_chain.support.present_modes));
	}
}

choose_surface_format :: proc(using ctx: ^Context) -> vk.SurfaceFormatKHR
{
	for v in swap_chain.support.formats
	{
		if v.format == .B8G8R8A8_SRGB && v.colorSpace == .SRGB_NONLINEAR do return v;
	}

	return swap_chain.support.formats[0];
}

choose_present_mode :: proc(using ctx: ^Context) -> vk.PresentModeKHR
{
	for v in swap_chain.support.present_modes
	{
		if v == .MAILBOX do return v;
	}

	return .FIFO;
}

choose_swap_extent :: proc(using ctx: ^Context) -> vk.Extent2D
{
	if (swap_chain.support.capabilities.currentExtent.width != max(u32))
	{
		return swap_chain.support.capabilities.currentExtent;
	}
	else
	{
		width, height := glfw.GetFramebufferSize(window.handle);

		extent := vk.Extent2D{u32(width), u32(height)};

		extent.width = clamp(extent.width, swap_chain.support.capabilities.minImageExtent.width, swap_chain.support.capabilities.maxImageExtent.width);
		extent.height = clamp(extent.height, swap_chain.support.capabilities.minImageExtent.height, swap_chain.support.capabilities.maxImageExtent.height);

		return extent;
	}
}

create_swap_chain :: proc(using ctx: ^Context)
{
	using ctx.swap_chain.support;
	swap_chain.format       = choose_surface_format(ctx);
	swap_chain.present_mode = choose_present_mode(ctx);
	swap_chain.extent       = choose_swap_extent(ctx);
	swap_chain.image_count  = capabilities.minImageCount + 1;

	if capabilities.maxImageCount > 0 && swap_chain.image_count > capabilities.maxImageCount
	{
		swap_chain.image_count = capabilities.maxImageCount;
	}

	create_info: vk.SwapchainCreateInfoKHR;
	create_info.sType = .SWAPCHAIN_CREATE_INFO_KHR;
	create_info.surface = surface;
	create_info.minImageCount = swap_chain.image_count;
	create_info.imageFormat = swap_chain.format.format;
	create_info.imageColorSpace = swap_chain.format.colorSpace;
	create_info.imageExtent = swap_chain.extent;
	create_info.imageArrayLayers = 1;
	create_info.imageUsage = {.COLOR_ATTACHMENT};

	queue_family_indices := [len(QueueFamily)]u32{u32(queue_indices[.Graphics]), u32(queue_indices[.Present])};

	if queue_indices[.Graphics] != queue_indices[.Present]
	{
		create_info.imageSharingMode = .CONCURRENT;
		create_info.queueFamilyIndexCount = 2;
		create_info.pQueueFamilyIndices = &queue_family_indices[0];
	}
	else
	{
		create_info.imageSharingMode = .EXCLUSIVE;
		create_info.queueFamilyIndexCount = 0;
		create_info.pQueueFamilyIndices = nil;
	}

	create_info.preTransform = capabilities.currentTransform;
	create_info.compositeAlpha = {.OPAQUE};
	create_info.presentMode = swap_chain.present_mode;
	create_info.clipped = true;
	create_info.oldSwapchain = vk.SwapchainKHR{};

	if res := vk.CreateSwapchainKHR(device, &create_info, nil, &swap_chain.handle); res != .SUCCESS
	{
		fmt.eprintf("Error: failed to create swap chain!\n");
		os.exit(1);
	}

	vk.GetSwapchainImagesKHR(device, swap_chain.handle, &swap_chain.image_count, nil);
	swap_chain.images = make([]vk.Image, swap_chain.image_count);
	vk.GetSwapchainImagesKHR(device, swap_chain.handle, &swap_chain.image_count, raw_data(swap_chain.images));
}

create_image_view :: proc(using ctx: ^Context, img: vk.Image, format: vk.Format, aspect_mask: vk.ImageAspectFlags) -> vk.ImageView
{
	create_info: vk.ImageViewCreateInfo;
	create_info.sType = .IMAGE_VIEW_CREATE_INFO;
	create_info.image = img;
	create_info.viewType = .D2;
	create_info.format = format;
	create_info.components.r = .IDENTITY;
	create_info.components.g = .IDENTITY;
	create_info.components.b = .IDENTITY;
	create_info.components.a = .IDENTITY;
	create_info.subresourceRange.aspectMask = aspect_mask;
	create_info.subresourceRange.baseMipLevel = 0;
	create_info.subresourceRange.levelCount = 1;
	create_info.subresourceRange.baseArrayLayer = 0;
	create_info.subresourceRange.layerCount = 1;

	view: vk.ImageView;
	if res := vk.CreateImageView(device, &create_info, nil, &view); res != .SUCCESS
	{
		fmt.eprintf("Error: failed to create image view!");
		os.exit(1);
	}

	return view;
}

create_image_views :: proc(using ctx: ^Context)
{
	using ctx.swap_chain;

	image_views = make([]vk.ImageView, len(images));

	for _, i in images
	{
		image_views[i] = create_image_view(ctx, images[i], format.format, {.COLOR});
	}
}

create_graphics_pipeline :: proc(using ctx: ^Context, vs_name: string, fs_name: string)
{
	vs_code := compile_shader(vs_name, .vertex_shader);
	fs_code := compile_shader(fs_name, .fragment_shader);
	defer
	{
		delete(vs_code);
		delete(fs_code);
	}

	vs_shader := create_shader_module(ctx, vs_code);
	fs_shader := create_shader_module(ctx, fs_code);
	defer
	{
		vk.DestroyShaderModule(device, vs_shader, nil);
		vk.DestroyShaderModule(device, fs_shader, nil);
	}

	vs_info := vk.PipelineShaderStageCreateInfo{
		sType = .PIPELINE_SHADER_STAGE_CREATE_INFO,
		stage = {.VERTEX},
		module = vs_shader,
		pName = "main",
	};

	fs_info := vk.PipelineShaderStageCreateInfo{
		sType = .PIPELINE_SHADER_STAGE_CREATE_INFO,
		stage = {.FRAGMENT},
		module = fs_shader,
		pName = "main",
	};

	shader_stages := [?]vk.PipelineShaderStageCreateInfo{vs_info, fs_info};

	dynamic_states := [?]vk.DynamicState{.VIEWPORT, .SCISSOR};
	dynamic_state := vk.PipelineDynamicStateCreateInfo{
		sType = .PIPELINE_DYNAMIC_STATE_CREATE_INFO,
		dynamicStateCount = len(dynamic_states),
		pDynamicStates = &dynamic_states[0],
	};

	vertex_input := vk.PipelineVertexInputStateCreateInfo{
		sType = .PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO,
		vertexBindingDescriptionCount = 1,
		pVertexBindingDescriptions = &VERTEX_BINDING,
		vertexAttributeDescriptionCount = len(VERTEX_ATTRIBUTES),
		pVertexAttributeDescriptions = &VERTEX_ATTRIBUTES[0],
	};

	input_assembly := vk.PipelineInputAssemblyStateCreateInfo{
		sType = .PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,
		topology = .TRIANGLE_LIST,
		primitiveRestartEnable = false,
	};

	viewport := vk.Viewport{
		x = 0.0,
		y = 0.0,
		width = cast(f32)swap_chain.extent.width,
		height = cast(f32)swap_chain.extent.height,
		minDepth = 0.0,
		maxDepth = 1.0,
	};

	scissor := vk.Rect2D{
		offset = {0, 0},
		extent = swap_chain.extent,
	};

	viewport_state := vk.PipelineViewportStateCreateInfo{
		sType = .PIPELINE_VIEWPORT_STATE_CREATE_INFO,
		viewportCount = 1,
		scissorCount = 1,
	};

	rasterizer := vk.PipelineRasterizationStateCreateInfo{
		sType = .PIPELINE_RASTERIZATION_STATE_CREATE_INFO,
		depthClampEnable = false,
		rasterizerDiscardEnable = false,
		polygonMode = .FILL,
		lineWidth = 1.0,
		cullMode = {.BACK},
		frontFace = .COUNTER_CLOCKWISE,
		depthBiasEnable = false,
		depthBiasConstantFactor = 0.0,
		depthBiasClamp = 0.0,
		depthBiasSlopeFactor = 0.0,
	};

	multisampling := vk.PipelineMultisampleStateCreateInfo{
		sType = .PIPELINE_MULTISAMPLE_STATE_CREATE_INFO,
		sampleShadingEnable = false,
		rasterizationSamples = {._1},
		minSampleShading = 1.0,
		pSampleMask = nil,
		alphaToCoverageEnable = false,
		alphaToOneEnable = false,
	};

	color_blend_attachment := vk.PipelineColorBlendAttachmentState{
		colorWriteMask = {.R, .G, .B, .A},
		blendEnable = true,
		srcColorBlendFactor = .SRC_ALPHA,
		dstColorBlendFactor = .ONE_MINUS_SRC_ALPHA,
		colorBlendOp = .ADD,
		srcAlphaBlendFactor = .ONE,
		dstAlphaBlendFactor = .ZERO,
		alphaBlendOp = .ADD,
	};

	color_blending := vk.PipelineColorBlendStateCreateInfo{
		sType = .PIPELINE_COLOR_BLEND_STATE_CREATE_INFO,
		logicOpEnable = false,
		logicOp = .COPY,
		attachmentCount = 1,
		pAttachments = &color_blend_attachment,
		blendConstants = {
			0 = 0.0,
			1 = 0.0,
			2 = 0.0,
			3 = 0.0,
		},
	};

	depth_stencil := vk.PipelineDepthStencilStateCreateInfo{
		sType = .PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO,
		depthTestEnable = true,
		depthWriteEnable = true,
		depthCompareOp = .LESS,
		depthBoundsTestEnable = false,
		minDepthBounds = 0.0,
		maxDepthBounds = 1.0,
		stencilTestEnable = false,
		front = {},
		back = {},
	};

	pipeline_layout_info := vk.PipelineLayoutCreateInfo{
		sType = .PIPELINE_LAYOUT_CREATE_INFO,
		setLayoutCount = 1,
		pSetLayouts = &descriptor_set_layout,
		pushConstantRangeCount = 0,
		pPushConstantRanges = nil,
	};

	if res := vk.CreatePipelineLayout(device, &pipeline_layout_info, nil, &pipeline.layout); res != .SUCCESS
	{
		fmt.eprintf("Error: Failed to create pipeline layout!\n");
		os.exit(1);
	}

	create_render_pass(ctx);

	pipeline_info := vk.GraphicsPipelineCreateInfo{
		sType = .GRAPHICS_PIPELINE_CREATE_INFO,
		stageCount = 2,
		pStages = &shader_stages[0],
		pVertexInputState = &vertex_input,
		pInputAssemblyState = &input_assembly,
		pViewportState = &viewport_state,
		pRasterizationState = &rasterizer,
		pMultisampleState = &multisampling,
		pDepthStencilState = &depth_stencil,
		pColorBlendState = &color_blending,
		pDynamicState = &dynamic_state,
		layout = pipeline.layout,
		renderPass = pipeline.render_pass,
		subpass = 0,
		basePipelineHandle = vk.Pipeline{},
		basePipelineIndex = -1,
	};

	if res := vk.CreateGraphicsPipelines(device, 0, 1, &pipeline_info, nil, &pipeline.handle); res != .SUCCESS
	{
		fmt.eprintf("Error: Failed to create graphics pipeline!\n");
		os.exit(1);
	}
}

create_shader_module :: proc(using ctx: ^Context, code: []u8) -> vk.ShaderModule
{
	create_info: vk.ShaderModuleCreateInfo;
	create_info.sType = .SHADER_MODULE_CREATE_INFO;
	create_info.codeSize = len(code);
	create_info.pCode = cast(^u32)raw_data(code);

	shader: vk.ShaderModule;
	if res := vk.CreateShaderModule(device, &create_info, nil, &shader); res != .SUCCESS
	{
		fmt.eprintf("Error: Could not create shader module!\n");
		os.exit(1);
	}

	return shader;
}

create_render_pass :: proc(using ctx: ^Context)
{
	color_attachment := vk.AttachmentDescription{
		format = swap_chain.format.format,
		samples = {._1},
		loadOp = .CLEAR,
		storeOp = .STORE,
		stencilLoadOp = .DONT_CARE,
		stencilStoreOp = .DONT_CARE,
		initialLayout = .UNDEFINED,
		finalLayout = .PRESENT_SRC_KHR,
	};

	color_attachment_ref := vk.AttachmentReference{
		attachment = 0,
		layout = .COLOR_ATTACHMENT_OPTIMAL,
	};

	depth_attachment := vk.AttachmentDescription{
		format = swap_chain.depth_image.format,
		samples = {._1},
		loadOp = .CLEAR,
		storeOp = .DONT_CARE,
		stencilLoadOp = .DONT_CARE,
		stencilStoreOp = .DONT_CARE,
		initialLayout = .UNDEFINED,
		finalLayout = .DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
	};

	depth_attachment_ref := vk.AttachmentReference{
		attachment = 1,
		layout = .DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
	}

	subpass := vk.SubpassDescription{
		pipelineBindPoint = .GRAPHICS,
		colorAttachmentCount = 1,
		pColorAttachments = &color_attachment_ref,
		pDepthStencilAttachment = &depth_attachment_ref,
	};

	dependency := vk.SubpassDependency{
		srcSubpass = vk.SUBPASS_EXTERNAL,
		dstSubpass = 0,
		srcStageMask = {.COLOR_ATTACHMENT_OUTPUT, .EARLY_FRAGMENT_TESTS},
		srcAccessMask = {},
		dstStageMask = {.COLOR_ATTACHMENT_OUTPUT, .EARLY_FRAGMENT_TESTS},
		dstAccessMask = {.COLOR_ATTACHMENT_WRITE, .DEPTH_STENCIL_ATTACHMENT_WRITE},
	};

	attachments := [?]vk.AttachmentDescription{color_attachment, depth_attachment};
	render_pass_info := vk.RenderPassCreateInfo{
		sType = .RENDER_PASS_CREATE_INFO,
		attachmentCount = len(attachments),
		pAttachments = &attachments[0],
		subpassCount = 1,
		pSubpasses = &subpass,
		dependencyCount = 1,
		pDependencies = &dependency,
	};

	if res := vk.CreateRenderPass(device, &render_pass_info, nil, &pipeline.render_pass); res != .SUCCESS
	{
		fmt.eprintf("Error: Failed to create render pass!\n");
		os.exit(1);
	}
}

create_framebuffers :: proc(using ctx: ^Context)
{
	swap_chain.framebuffers = make([]vk.Framebuffer, len(swap_chain.image_views));
	for v, i in swap_chain.image_views
	{
		attachments := [?]vk.ImageView{v, swap_chain.depth_image.view};

		framebuffer_info := vk.FramebufferCreateInfo{
			sType = .FRAMEBUFFER_CREATE_INFO,
			renderPass = pipeline.render_pass,
			attachmentCount = len(attachments),
			pAttachments = &attachments[0],
			width = swap_chain.extent.width,
			height = swap_chain.extent.height,
			layers = 1,
		};

		if res := vk.CreateFramebuffer(device, &framebuffer_info, nil, &swap_chain.framebuffers[i]); res != .SUCCESS
		{
			fmt.eprintf("Error: Failed to create framebuffer #%d!\n", i);
			os.exit(1);
		}
	}
}

create_command_pool :: proc(using ctx: ^Context)
{
	pool_info: vk.CommandPoolCreateInfo;
	pool_info.sType = .COMMAND_POOL_CREATE_INFO;
	pool_info.flags = {.RESET_COMMAND_BUFFER};
	pool_info.queueFamilyIndex = u32(queue_indices[.Graphics]);

	if res := vk.CreateCommandPool(device, &pool_info, nil, &command_pool); res != .SUCCESS
	{
		fmt.eprintf("Error: Failed to create command pool!\n");
		os.exit(1);
	}
}

create_command_buffers :: proc(using ctx: ^Context)
{
	alloc_info: vk.CommandBufferAllocateInfo;
	alloc_info.sType = .COMMAND_BUFFER_ALLOCATE_INFO;
	alloc_info.commandPool = command_pool;
	alloc_info.level = .PRIMARY;
	alloc_info.commandBufferCount = len(command_buffers);

	if res := vk.AllocateCommandBuffers(device, &alloc_info, &command_buffers[0]); res != .SUCCESS
	{
		fmt.eprintf("Error: Failed to allocate command buffers!\n");
		os.exit(1);
	}
}

create_sync_objects :: proc(using ctx: ^Context)
{
	semaphore_info: vk.SemaphoreCreateInfo;
	semaphore_info.sType = .SEMAPHORE_CREATE_INFO;

	fence_info: vk.FenceCreateInfo;
	fence_info.sType = .FENCE_CREATE_INFO;
	fence_info.flags = {.SIGNALED}

	for i in 0..<MAX_FRAMES_IN_FLIGHT
	{
		res := vk.CreateSemaphore(device, &semaphore_info, nil, &image_available[i]);
		if res != .SUCCESS
		{
			fmt.eprintf("Error: Failed to create \"image_available\" semaphore\n");
			os.exit(1);
		}
		res = vk.CreateSemaphore(device, &semaphore_info, nil, &render_finished[i]);
		if res != .SUCCESS
		{
			fmt.eprintf("Error: Failed to create \"render_finished\" semaphore\n");
			os.exit(1);
		}
		res = vk.CreateFence(device, &fence_info, nil, &in_flight[i]);
		if res != .SUCCESS
		{
			fmt.eprintf("Error: Failed to create \"in_flight\" fence\n");
			os.exit(1);
		}
	}
}

recreate_swap_chain :: proc(using ctx: ^Context)
{
	width, height := glfw.GetFramebufferSize(window.handle);
	for width == 0 && height == 0
	{
		width, height = glfw.GetFramebufferSize(window.handle);
		glfw.WaitEvents();
	}

	window.res.x = uint(width);
	window.res.y = uint(height);
	vk.DeviceWaitIdle(device);

	query_swap_chain_details(ctx, physical_device);

	cleanup_swap_chain(ctx);

	create_swap_chain(ctx);
	create_image_views(ctx);
	create_depth_resources(ctx);
	create_framebuffers(ctx);
}

cleanup_swap_chain :: proc(using ctx: ^Context)
{
	vk.DestroyImageView(device, swap_chain.depth_image.view, nil);
	vk.DestroyImage(device, swap_chain.depth_image.handle, nil);
	vk.FreeMemory(device, swap_chain.depth_image.memory, nil);

	for f in swap_chain.framebuffers
	{
		vk.DestroyFramebuffer(device, f, nil);
	}
	for view in swap_chain.image_views
	{
		vk.DestroyImageView(device, view, nil);
	}
	vk.DestroySwapchainKHR(device, swap_chain.handle, nil);
}

destroy_vkbuffer :: proc(using ctx: ^Context, buffer: VKBuffer)
{
	vk.FreeMemory(device, buffer.memory, nil);
	vk.DestroyBuffer(device, buffer.buffer, nil);
}

destroy_texture :: proc(using ctx: ^Context, tex: Image)
{
	vk.DestroyImageView(device, tex.view, nil);
	vk.DestroyImage(device, tex.handle, nil);
	vk.FreeMemory(device, tex.memory, nil);
}

create_vertex_buffer :: proc(using ctx: ^Context, vertices: []Vertex) -> VKBuffer
{
	staging: VKBuffer;
	ret: VKBuffer;
	create_vkbuffer(ctx, size_of(Vertex), len(vertices), {.TRANSFER_SRC}, {.HOST_VISIBLE, .HOST_COHERENT}, &staging);

	data: rawptr;
	vk.MapMemory(device, staging.memory, 0, staging.size, {}, &data);
	mem.copy(data, raw_data(vertices), cast(int)staging.size);
	vk.UnmapMemory(device, staging.memory);

	create_vkbuffer(ctx, size_of(Vertex), len(vertices), {.VERTEX_BUFFER, .TRANSFER_DST}, {.DEVICE_LOCAL}, &ret);
	copy_vkbuffer(ctx, staging, ret, ret.size);

	destroy_vkbuffer(ctx, staging);

	return ret;
}

create_index_buffer :: proc(using ctx: ^Context, indices: []u16) -> VKBuffer
{
	staging: VKBuffer;
	ret: VKBuffer;
	create_vkbuffer(ctx, size_of(indices[0]), len(indices), {.TRANSFER_SRC}, {.HOST_VISIBLE, .HOST_COHERENT}, &staging);

	data: rawptr;
	vk.MapMemory(device, staging.memory, 0, staging.size, {}, &data);
	mem.copy(data, raw_data(indices), cast(int)staging.size);
	vk.UnmapMemory(device, staging.memory);

	create_vkbuffer(ctx, size_of(indices[0]), len(indices), {.INDEX_BUFFER, .TRANSFER_DST}, {.DEVICE_LOCAL}, &ret);
	copy_vkbuffer(ctx, staging, ret, ret.size);

	destroy_vkbuffer(ctx, staging);

	return ret;
}

create_uniform_buffers :: proc(using ctx: ^Context)
{
	buffer_size := size_of(Uniform_Buffer_Object);

	for b in &uniform_buffers
	{
		create_vkbuffer(ctx, buffer_size, 1, {.UNIFORM_BUFFER}, {.HOST_VISIBLE, .HOST_COHERENT}, &b);
	}
}

copy_vkbuffer :: proc(using ctx: ^Context, src, dst: VKBuffer, size: vk.DeviceSize)
{
	cmd_buffer := one_time_commands(ctx);

	copy_region := vk.BufferCopy{
		srcOffset = 0,
		dstOffset = 0,
		size = size,
	}
	vk.CmdCopyBuffer(cmd_buffer, src.buffer, dst.buffer, 1, &copy_region);
}

find_memory_type :: proc(using ctx: ^Context, type_filter: u32, properties: vk.MemoryPropertyFlags) -> u32
{
	mem_properties: vk.PhysicalDeviceMemoryProperties;
	vk.GetPhysicalDeviceMemoryProperties(physical_device, &mem_properties);
	for i in 0..<mem_properties.memoryTypeCount
	{
		if (type_filter & (1 << i) != 0) && (mem_properties.memoryTypes[i].propertyFlags & properties) == properties
		{
			return i;
		}
	}

	fmt.eprintf("Error: Failed to find suitable memory type!\n");
	os.exit(1);
}

create_vkbuffer :: proc(using ctx: ^Context, member_size: int, count: int, usage: vk.BufferUsageFlags, properties: vk.MemoryPropertyFlags, buffer: ^VKBuffer)
{
	buffer.length = count;
	buffer.size = cast(vk.DeviceSize)(member_size * count);

	buffer_info := vk.BufferCreateInfo{
		sType = .BUFFER_CREATE_INFO,
		size  = buffer.size,
		usage = usage,
		sharingMode = .EXCLUSIVE,
	};

	if res := vk.CreateBuffer(device, &buffer_info, nil, &buffer.buffer); res != .SUCCESS
	{
		fmt.eprintf("Error: failed to create buffer\n");
		os.exit(1);
	}

	mem_requirements: vk.MemoryRequirements;
	vk.GetBufferMemoryRequirements(device, buffer.buffer, &mem_requirements);

	alloc_info := vk.MemoryAllocateInfo{
		sType = .MEMORY_ALLOCATE_INFO,
		allocationSize = mem_requirements.size,
		memoryTypeIndex = find_memory_type(ctx, mem_requirements.memoryTypeBits, {.HOST_VISIBLE, .HOST_COHERENT})
	};

	if res := vk.AllocateMemory(device, &alloc_info, nil, &buffer.memory); res != .SUCCESS
	{
		fmt.eprintf("Error: Failed to allocate buffer memory!\n");
		os.exit(1);
	}

	vk.BindBufferMemory(device, buffer.buffer, buffer.memory, 0);
}

create_descriptor_set_layout :: proc(using ctx: ^Context)
{
	ubo_layout_binding := vk.DescriptorSetLayoutBinding{
		binding = 0,
		descriptorType = .UNIFORM_BUFFER,
		descriptorCount = 1,
		stageFlags = {.VERTEX},
		pImmutableSamplers = nil,
	};

	sampler_layout_binding := vk.DescriptorSetLayoutBinding{
		binding = 1,
		descriptorType = .COMBINED_IMAGE_SAMPLER,
		descriptorCount = 1,
		stageFlags = {.FRAGMENT},
		pImmutableSamplers = nil,
	};

	bindings := [?]vk.DescriptorSetLayoutBinding{ubo_layout_binding, sampler_layout_binding};
	layout_info := vk.DescriptorSetLayoutCreateInfo{
		sType = .DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
		bindingCount = len(bindings),
		pBindings = &bindings[0],
	};

	if res := vk.CreateDescriptorSetLayout(device, &layout_info, nil, &descriptor_set_layout); res != .SUCCESS
	{
		fmt.eprintf("Error: Failed to create descriptor set layout!\n");
		os.exit(1);
	}
}

update_uniform_buffer :: proc(using ctx: ^Context)
{
	ubo: Uniform_Buffer_Object;
	ubo.model = glsl.identity(glsl.mat4);
	//ubo.model = glsl.mat4Translate({0, math.floor(math.sin(f32(time.duration_milliseconds(timer.total))/1000)*(0.1*f32(window.res.y))), 0});
	ubo.res = {cast(u32)window.res.x, cast(u32)window.res.y};

	data: rawptr;
	vk.MapMemory(device, uniform_buffers[curr_frame].memory, 0, size_of(ubo), {}, &data);
	mem.copy(data, &ubo, size_of(ubo));
	vk.UnmapMemory(device, uniform_buffers[curr_frame].memory);
}

create_descriptor_pool :: proc(using ctx: ^Context)
{
	pool_sizes := [2]vk.DescriptorPoolSize{
		0 = {
			type = .UNIFORM_BUFFER,
			descriptorCount = MAX_FRAMES_IN_FLIGHT,
		},
		1 = {
			type = .COMBINED_IMAGE_SAMPLER,
			descriptorCount = MAX_FRAMES_IN_FLIGHT,
		}
	};

	pool_info := vk.DescriptorPoolCreateInfo{
		sType = .DESCRIPTOR_POOL_CREATE_INFO,
		poolSizeCount = len(pool_sizes),
		pPoolSizes = &pool_sizes[0],
		maxSets = MAX_FRAMES_IN_FLIGHT,
	};

	if res := vk.CreateDescriptorPool(device, &pool_info, nil, &descriptor_pool); res != .SUCCESS
	{
		fmt.eprintf("Error: Failed to create descriptor pool\n");
		os.exit(1);
	}
}

create_descriptor_set :: proc(using ctx: ^Context)
{
	layouts: [MAX_FRAMES_IN_FLIGHT]vk.DescriptorSetLayout;
	for l in &layouts do l = descriptor_set_layout;

	alloc_info := vk.DescriptorSetAllocateInfo{
		sType = .DESCRIPTOR_SET_ALLOCATE_INFO,
		descriptorPool = descriptor_pool,
		descriptorSetCount = MAX_FRAMES_IN_FLIGHT,
		pSetLayouts = &layouts[0],
	};

	if res := vk.AllocateDescriptorSets(device, &alloc_info, &descriptor_sets[0]); res != .SUCCESS
	{
		fmt.eprintf("Error: Failed to allocate descriptor sets!\n");
		os.exit(1);
	}

	for i in 0..<MAX_FRAMES_IN_FLIGHT
	{
		buffer_info := vk.DescriptorBufferInfo{
			buffer = uniform_buffers[i].buffer,
			offset = 0,
			range  = uniform_buffers[i].size,
		};

		descriptor_write := vk.WriteDescriptorSet{
			sType = .WRITE_DESCRIPTOR_SET,
			dstSet = descriptor_sets[i],
			dstBinding = 0,
			dstArrayElement = 0,
			descriptorType = .UNIFORM_BUFFER,
			descriptorCount = 1,
			pBufferInfo = &buffer_info,
			pImageInfo = nil,
			pTexelBufferView = nil,
		};

		vk.UpdateDescriptorSets(device, 1, &descriptor_write, 0, nil);
	}
}

create_image ::proc(using ctx: ^Context, width, height: u32, format: vk.Format, tiling: vk.ImageTiling, usage: vk.ImageUsageFlags, properties: vk.MemoryPropertyFlags) -> Image
{
	img: Image;
	img.dim = {width, height};
	img.format = format;

	image_info := vk.ImageCreateInfo{
		sType = .IMAGE_CREATE_INFO,
		imageType = .D2,
		extent = {width, height, 1},
		mipLevels = 1,
		arrayLayers = 1,
		format = format,
		tiling = tiling,
		initialLayout = .UNDEFINED,
		usage = usage,
		sharingMode = .EXCLUSIVE,
		samples = {._1},
		flags = {},
	};

	if res := vk.CreateImage(device, &image_info, nil, &img.handle); res != .SUCCESS
	{
		fmt.eprintf("Error: Failed to create image\n");
		os.exit(1);
	}

	mem_requirements: vk.MemoryRequirements;
	vk.GetImageMemoryRequirements(device, img.handle, &mem_requirements);

	alloc_info := vk.MemoryAllocateInfo{
		sType = .MEMORY_ALLOCATE_INFO,
		allocationSize = mem_requirements.size,
		memoryTypeIndex = find_memory_type(ctx, mem_requirements.memoryTypeBits, properties),
	};

	if res := vk.AllocateMemory(device, &alloc_info, nil, &img.memory); res != .SUCCESS
	{
		fmt.eprintf("Error: Failed to allocate memory\n");
		os.exit(1);
	}

	vk.BindImageMemory(device, img.handle, img.memory, 0);

	return img;
}

create_texture :: proc{create_texture_from_path, create_texture_from_mem};
create_texture_from_path :: proc(using ctx: ^Context, path: string) -> Image
{
	img, err := image.load(path);
	if err != nil
	{
		fmt.eprintf("Error: failed to load image %q\n", path);
		os.exit(1);
	}

	defer image.destroy(img);
	bpp := (img.channels * img.depth) / 8;



	return create_texture_from_mem(ctx, img.pixels.buf[:], img.width, img.height, bpp, .R8G8B8A8_SRGB);
}

create_texture_from_mem :: proc(using ctx: ^Context, pixels: []byte, width, height, bpp: int, format: vk.Format) -> Image
{
	staging_buffer: VKBuffer;
	create_vkbuffer(ctx, bpp, width*height, {.TRANSFER_SRC}, {.HOST_VISIBLE, .HOST_COHERENT}, &staging_buffer);
	defer
	{
		vk.DestroyBuffer(device, staging_buffer.buffer, nil);
		vk.FreeMemory(device, staging_buffer.memory, nil);
	}
	data: rawptr;
	vk.MapMemory(device, staging_buffer.memory, 0, cast(vk.DeviceSize)(bpp*width*height), {}, &data);
	mem.copy(data, raw_data(pixels), bpp*width*height);
	vk.UnmapMemory(device, staging_buffer.memory);

	texture : = create_image(ctx, cast(u32)width, cast(u32)height, format, .OPTIMAL, {.TRANSFER_DST, .SAMPLED}, {.DEVICE_LOCAL});

	transition_image_layout(ctx, texture.handle, texture.format, .UNDEFINED, .TRANSFER_DST_OPTIMAL);
	copy_buffer_to_image(ctx, staging_buffer, texture.handle, u32(width), u32(height));
	transition_image_layout(ctx, texture.handle, texture.format, .TRANSFER_DST_OPTIMAL, .SHADER_READ_ONLY_OPTIMAL);

	texture.view = create_image_view(ctx, texture.handle, texture.format, {.COLOR});

	return texture;
}

@(deferred_in_out=end_one_time_commands)
one_time_commands :: proc(using ctx: ^Context) -> vk.CommandBuffer
{
	return begin_one_time_commands(ctx);
}

begin_one_time_commands :: proc(using ctx: ^Context) -> vk.CommandBuffer
{
	alloc_info := vk.CommandBufferAllocateInfo{
		sType = .COMMAND_BUFFER_ALLOCATE_INFO,
		level = .PRIMARY,
		commandPool = command_pool,
		commandBufferCount = 1,
	};

	buffer: vk.CommandBuffer;
	vk.AllocateCommandBuffers(device, &alloc_info, &buffer);

	begin_info := vk.CommandBufferBeginInfo{
		sType = .COMMAND_BUFFER_BEGIN_INFO,
		flags = {.ONE_TIME_SUBMIT},
	};

	vk.BeginCommandBuffer(buffer, &begin_info);

	return buffer;
}

end_one_time_commands :: proc(using ctx: ^Context, buffer: vk.CommandBuffer)
{
	buffer := buffer;
	vk.EndCommandBuffer(buffer);

	submit_info := vk.SubmitInfo{
		sType = .SUBMIT_INFO,
		commandBufferCount = 1,
		pCommandBuffers = &buffer,
	};

	vk.QueueSubmit(queues[.Graphics], 1, &submit_info, {});
	vk.QueueWaitIdle(queues[.Graphics]);

	vk.FreeCommandBuffers(device, command_pool, 1, &buffer);
}

transition_image_layout :: proc(using ctx: ^Context, img: vk.Image, format: vk.Format, old_layout, new_layout: vk.ImageLayout)
{
	buffer := one_time_commands(ctx);

	barrier := vk.ImageMemoryBarrier{
		sType = .IMAGE_MEMORY_BARRIER,
		oldLayout = old_layout,
		newLayout = new_layout,
		srcQueueFamilyIndex = max(u32),
		dstQueueFamilyIndex = max(u32),
		image = img,
		subresourceRange = {
			baseMipLevel = 0,
			levelCount = 1,
			baseArrayLayer = 0,
			layerCount = 1,
		},
		srcAccessMask = {},
		dstAccessMask = {},
	};

	src, dst: vk.PipelineStageFlags;
	if old_layout == .UNDEFINED && new_layout == .TRANSFER_DST_OPTIMAL
	{
		barrier.dstAccessMask = {.TRANSFER_WRITE};
		src = {.TOP_OF_PIPE};
		dst = {.TRANSFER};
	}
	else if old_layout == .TRANSFER_DST_OPTIMAL && new_layout == .SHADER_READ_ONLY_OPTIMAL
	{
		barrier.srcAccessMask = {.TRANSFER_WRITE};
		barrier.dstAccessMask = {.SHADER_READ};
		src = {.TRANSFER};
		dst = {.FRAGMENT_SHADER};
	}
	else if old_layout == .UNDEFINED && new_layout == .DEPTH_STENCIL_ATTACHMENT_OPTIMAL
	{
		barrier.srcAccessMask = {};
		barrier.dstAccessMask = {.DEPTH_STENCIL_ATTACHMENT_READ, .DEPTH_STENCIL_ATTACHMENT_WRITE};
		src = {.TOP_OF_PIPE};
		dst = {.EARLY_FRAGMENT_TESTS};
	}

	if new_layout == .DEPTH_STENCIL_ATTACHMENT_OPTIMAL
	{
		barrier.subresourceRange.aspectMask = {.DEPTH};

		if format != .D32_SFLOAT
		{
			barrier.subresourceRange.aspectMask |= {.STENCIL};
		}
	}
	else
	{
		barrier.subresourceRange.aspectMask = {.COLOR};
	}

	vk.CmdPipelineBarrier(buffer,
						  src, dst,
						  {},
						  0, nil,
						  0, nil,
						  1, &barrier);


}

copy_buffer_to_image :: proc(using ctx: ^Context, buffer: VKBuffer, img: vk.Image, width, height: u32)
{
	cmd_buffer := one_time_commands(ctx);

	region := vk.BufferImageCopy{
		bufferOffset = 0,
		bufferRowLength = 0,
		bufferImageHeight = 0,
		imageSubresource = {
			aspectMask = {.COLOR},
			mipLevel = 0,
			baseArrayLayer = 0,
			layerCount = 1,
		},

		imageOffset = {0, 0, 0},
		imageExtent = {width, height, 1},
	};

	vk.CmdCopyBufferToImage(cmd_buffer, buffer.buffer, img, .TRANSFER_DST_OPTIMAL, 1, &region);
}

create_texture_samplers :: proc(using ctx: ^Context)
{
	properties: vk.PhysicalDeviceProperties;
	vk.GetPhysicalDeviceProperties(physical_device, &properties);

	for sampler in &samplers
	{
		sampler_info := vk.SamplerCreateInfo{
			sType = .SAMPLER_CREATE_INFO,
			magFilter = .LINEAR,
			minFilter = .LINEAR,
			addressModeU = .REPEAT,
			addressModeV = .REPEAT,
			addressModeW = .REPEAT,
			anisotropyEnable = true,
			maxAnisotropy = properties.limits.maxSamplerAnisotropy,
			borderColor = .INT_OPAQUE_BLACK,
			unnormalizedCoordinates = false,
			compareEnable = false,
			compareOp = .ALWAYS,
			mipmapMode = .LINEAR,
			mipLodBias = 0,
			minLod = 0,
			maxLod = 0,
		};

		if res := vk.CreateSampler(device, &sampler_info, nil, &sampler); res != .SUCCESS
		{
			fmt.eprintf("Error: Failed to create texture sampler!\n");
			os.exit(1);
		}
	}
}

upload_texture :: proc(using ctx: ^Context, img: Image, idx: u32)
{
	for i in 0..<MAX_FRAMES_IN_FLIGHT
	{
		image_info := vk.DescriptorImageInfo{
			imageLayout = .SHADER_READ_ONLY_OPTIMAL,
			imageView = img.view,
			sampler = samplers[idx],
		};

		descriptor_write := vk.WriteDescriptorSet{
			sType = .WRITE_DESCRIPTOR_SET,
			dstSet = descriptor_sets[i],
			dstBinding = idx+1,
			dstArrayElement = 0,
			descriptorType = .COMBINED_IMAGE_SAMPLER,
			descriptorCount = 1,
			pImageInfo = &image_info,
		};

		vk.UpdateDescriptorSets(device, 1, &descriptor_write, 0, nil);
	}
}

create_depth_resources :: proc(using ctx: ^Context)
{
	depth_format := find_supported_format(ctx,
										  {.D32_SFLOAT, .D32_SFLOAT_S8_UINT, .D24_UNORM_S8_UINT},
										  .OPTIMAL,
										  {.DEPTH_STENCIL_ATTACHMENT});

	depth_image := create_image(ctx, swap_chain.extent.width, swap_chain.extent.height, depth_format, .OPTIMAL, {.DEPTH_STENCIL_ATTACHMENT}, {.DEVICE_LOCAL});
	depth_image.view = create_image_view(ctx, depth_image.handle, depth_format, {.DEPTH});
	transition_image_layout(ctx, depth_image.handle, depth_format, .UNDEFINED, .DEPTH_STENCIL_ATTACHMENT_OPTIMAL);

	swap_chain.depth_image = depth_image;
}

find_supported_format :: proc(using ctx: ^Context, candidates: []vk.Format, tiling: vk.ImageTiling, features: vk.FormatFeatureFlags) -> vk.Format
{
	for format in candidates
	{
		properties: vk.FormatProperties;
		vk.GetPhysicalDeviceFormatProperties(physical_device, format, &properties);

		if tiling == .LINEAR && properties.linearTilingFeatures >= features
		{
			return format
		}
		else if tiling == .OPTIMAL && properties.optimalTilingFeatures >= features
		{
			return format;
		}
	}

	fmt.eprintf("Error: Failed to find a supported format\n");
	os.exit(1);
}
